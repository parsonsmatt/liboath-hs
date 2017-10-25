
#include <oath.h>

int inline_c_LibOath_Internal_0_e5c09df3584bfac5d1724487f17015d119521c92() {
return ( oath_init() );
}


int inline_c_LibOath_Internal_1_a3a650f2b04aa545b11a0331d5430727896128b7() {
return ( oath_done() );
}


const char * inline_c_LibOath_Internal_2_09ad50aa4440ab320ce845ed83cbd60f5df78084(const char * minVersion_inline_c_0) {
return ( oath_check_version(minVersion_inline_c_0) );
}


const char * inline_c_LibOath_Internal_3_ffc6a8a5085d32853b6c7f73038d1c1e3ea3fbb2(int err_inline_c_0) {
return ( oath_strerror(err_inline_c_0) );
}


int inline_c_LibOath_Internal_4_2e7b1f4bb1c11117a98ae1f9462b140e1e6a32e1(const char * in__inline_c_0, size_t inlen_inline_c_1, char ** out_inline_c_2, size_t * outlen_inline_c_3) {
return ( 
        oath_base32_decode(in__inline_c_0, inlen_inline_c_1, out_inline_c_2, outlen_inline_c_3) 
    );
}


int inline_c_LibOath_Internal_5_a2e36bd27888e723adba5735702f90927e00ff28(const char * in__inline_c_0, size_t inlen_inline_c_1, char ** out_inline_c_2, size_t * outlen_inline_c_3) {
return ( 
        oath_base32_encode(in__inline_c_0, inlen_inline_c_1, out_inline_c_2, outlen_inline_c_3) 
    );
}


int inline_c_LibOath_Internal_6_169965319a18591060847b551fdf97ead7a1885f(const char * secret_inline_c_0, size_t secretLen_inline_c_1, time_t now_inline_c_2, unsigned timeStepSize_inline_c_3, time_t startOffset_inline_c_4, unsigned digits_inline_c_5, int flags_inline_c_6, char * outputOtp_inline_c_7) {
return (
        oath_totp_generate2(
            secret_inline_c_0, 
            secretLen_inline_c_1,
            now_inline_c_2,
            timeStepSize_inline_c_3,
            startOffset_inline_c_4,
            digits_inline_c_5,
            flags_inline_c_6,
            outputOtp_inline_c_7
        ) 
    );
}

