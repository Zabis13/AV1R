// R ↔ C++ bindings for AV1R
// CPU encoding: ffmpeg вызывается через system() в R-коде (нет линковки с libavcodec)
// GPU encoding: Vulkan через этот файл

#include <R.h>
#include <Rinternals.h>
#include <cstring>
#include <stdexcept>
#include <string>

#include "../inst/include/av1r.h"

#ifdef AV1R_USE_VULKAN
#include "av1r_vulkan_ctx.h"

// Forward declarations
VkInstance       av1r_create_instance();
void             av1r_destroy_instance(VkInstance);
int              av1r_device_count(VkInstance);
VkPhysicalDevice av1r_select_device(VkInstance, int);
bool             av1r_device_supports_av1_encode(VkPhysicalDevice);
void             av1r_device_name(VkPhysicalDevice, char*, int);
VkDevice         av1r_create_logical_device(VkPhysicalDevice, uint32_t*);
void             av1r_destroy_logical_device(VkDevice);
VkFence          av1r_create_fence(VkDevice);
VkCommandPool    av1r_create_command_pool(VkDevice, uint32_t);
#endif

// ============================================================================
// R_av1r_vulkan_available  →  logical(1)
// ============================================================================
extern "C" SEXP R_av1r_vulkan_available(void) {
#ifdef AV1R_VULKAN_VIDEO_AV1
    return Rf_ScalarLogical(TRUE);
#else
    return Rf_ScalarLogical(FALSE);
#endif
}

// ============================================================================
// R_av1r_vulkan_devices  →  character vector
// ============================================================================
extern "C" SEXP R_av1r_vulkan_devices(void) {
#ifdef AV1R_USE_VULKAN
    try {
        VkInstance inst = av1r_create_instance();
        int n = av1r_device_count(inst);
        SEXP result = PROTECT(Rf_allocVector(STRSXP, n));
        for (int i = 0; i < n; i++) {
            VkPhysicalDevice dev = av1r_select_device(inst, i);
            char name[256];
            av1r_device_name(dev, name, sizeof(name));
            bool av1_ok = av1r_device_supports_av1_encode(dev);
            std::string label = std::string(name) + (av1_ok ? " [AV1]" : "");
            SET_STRING_ELT(result, i, Rf_mkChar(label.c_str()));
        }
        UNPROTECT(1);
        av1r_destroy_instance(inst);
        return result;
    } catch (const std::exception& e) {
        Rf_warning("vulkan_devices: %s", e.what());
        return Rf_allocVector(STRSXP, 0);
    }
#else
    return Rf_allocVector(STRSXP, 0);
#endif
}

// ============================================================================
// R_av1r_detect_backend(prefer)  →  character(1): "vulkan" | "cpu"
// ============================================================================
extern "C" SEXP R_av1r_detect_backend(SEXP prefer) {
    const char* pref = CHAR(STRING_ELT(prefer, 0));
    if (strcmp(pref, "cpu") == 0) {
        return Rf_mkString("cpu");
    }
#ifdef AV1R_VULKAN_VIDEO_AV1
    try {
        VkInstance inst = av1r_create_instance();
        int n = av1r_device_count(inst);
        for (int i = 0; i < n; i++) {
            VkPhysicalDevice dev = av1r_select_device(inst, i);
            if (av1r_device_supports_av1_encode(dev)) {
                av1r_destroy_instance(inst);
                return Rf_mkString("vulkan");
            }
        }
        av1r_destroy_instance(inst);
    } catch (...) {}
#endif
    return Rf_mkString("cpu");
}

// ============================================================================
// Registration table
// ============================================================================
static const R_CallMethodDef CallEntries[] = {
    { "R_av1r_vulkan_available", (DL_FUNC) &R_av1r_vulkan_available, 0 },
    { "R_av1r_vulkan_devices",   (DL_FUNC) &R_av1r_vulkan_devices,   0 },
    { "R_av1r_detect_backend",   (DL_FUNC) &R_av1r_detect_backend,   1 },
    { nullptr, nullptr, 0 }
};

extern "C" void R_init_AV1R(DllInfo* dll) {
    R_registerRoutines(dll, nullptr, CallEntries, nullptr, nullptr);
    R_useDynamicSymbols(dll, FALSE);
}
