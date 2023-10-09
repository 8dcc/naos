
#ifndef KERNEL_FRAMEBUFFER_H_
#define KERNEL_FRAMEBUFFER_H_ 1

#include <stdint.h>
#include <kernel/color.h>
#include <kernel/font.h>

/**
 * @brief Framebuffer types returned by the bootloader
 * @details See bottom of:
 *   https://www.gnu.org/software/grub/manual/multiboot/html_node/Boot-information-format.html
 */
enum fb_types {
    FB_TYPE_INDEXED = 0,
    FB_TYPE_RGB     = 1, /**< @brief Requested */
    FB_TYPE_EGA     = 2,
};

/**
 * @struct GimpImage
 * @brief Structure containing the height, width and data of an image exported
 * by GIMP.
 */
typedef struct {
    uint32_t h, w;
    const char* data;
} GimpImage;

/**
 * @brief Initialize global framebuffer variables and clear the framebuffer
 * @details See Multiboot struct for more info
 * @param[out] fb Framebuffer address returned by the bootloader
 * @param[in] pitch Framebuffer pitch
 * @param[in] w Framebuffer width in px
 * @param[in] h Framebuffer height in px
 * @param[in] bpp Framebuffer bits per pixel
 */
void fb_init(volatile uint32_t* fb, uint32_t pitch, uint32_t w, uint32_t h,
             uint32_t bpp);

/**
 * @brief Get the framebuffer ptr.
 * @details Used by operations that need high performance like the fbc. Use with
 * caution.
 * @return Framebuffer address
 */
volatile uint32_t* fb_get_ptr(void);

/**
 * @brief Get the framebuffer width in px
 * @return Framebuffer width in px
 */
uint32_t fb_get_width(void);

/**
 * @brief Get the framebuffer height in px
 * @return Framebuffer height in px
 */
uint32_t fb_get_height(void);

/**
 * @brief Set the pixel at \p y, \p x of the global framebuffer to \p col
 * @param y, x Position in px of the framebuffer
 * @param col New 32 bit color for that px
 */
void fb_setpx_col(uint32_t y, uint32_t x, uint32_t col);

/**
 * @brief RGB wrapper for fb_setpx_col()
 * @param y, x Position in px of the framebuffer
 * @param r, g, b Color in RGB format
 */
static inline void fb_setpx(uint32_t y, uint32_t x, uint8_t r, uint8_t g,
                            uint8_t b) {
    fb_setpx_col(y, x, rgb2col(r, g, b));
}

/**
 * @brief Fill \p h and \p w starting from \p y and \p x with color \p col
 * @param y, x Position in px of the framebuffer
 * @param h, w Height and width of the rectangle
 * @param col New 32 bit color for the rectangle
 */
void fb_drawrect_col(uint32_t y, uint32_t x, uint32_t h, uint32_t w,
                     uint32_t col);

/**
 * @brief RGB wrapper for fb_drawrect_col()
 * @param y, x Position in px of the framebuffer
 * @param h, w Height and width of the rectangle
 * @param r, g, b Color in RGB format
 */
static inline void fb_drawrect(uint32_t y, uint32_t x, uint32_t h, uint32_t w,
                               uint8_t r, uint8_t g, uint8_t b) {
    fb_drawrect_col(y, x, h, w, rgb2col(r, g, b));
}

/**
 * @brief Same as fb_drawrect_col() but less secure (no bounds check).
 * @details Used when we know for sure we can control the parameters.
 * @param y, x Position in px of the framebuffer
 * @param h, w Height and width of the rectangle
 * @param col New 32 bit color for the rectangle
 */
void fb_drawrect_fast(uint32_t y, uint32_t x, uint32_t h, uint32_t w,
                      uint32_t col);

/**
 * @brief Draw text on the framebuffer
 * @param[in] y, x Top left position of the text on the screen
 * @param[in] cols Background/foreground color pair for the text
 * @param[out] font Font for the text
 * @param[in] s String to draw
 */
void fb_drawtext(uint32_t y, uint32_t x, color_pair cols, Font* font,
                 const char* s);

/**
 * @brief Draw image from C array exported by gimp.
 * @details For exporting an image to a C array, use:
 * File > Export As > Select File Type (By Extension) > C source code header
 * You will need to store the values generated by GIMP in a GimpImage struct.
 * @param[in] y, x Position in px for drawing the image
 * @param[in] img Pointer to the GimpImage to draw.
 */
void fb_drawimage(uint32_t y, uint32_t x, const GimpImage* img);

#endif /* KERNEL_FRAMEBUFFER_H_ */
