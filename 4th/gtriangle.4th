\ 4tH graphics Sierpinski's triangle - Copyright 2014 J.L. Bezemer
\ You can redistribute this file and/or modify it under
\ the terms of the GNU General Public License

\ NOTE: This program requires about 1.5 MB of memory
\ It won't run on 64K machines!!

include lib/graphics.4th               \ graphics support is needed

520 pic_width !                        \ width of the image
520 pic_height !                       \ height of the image

9 constant order                       \ Sierpinski's triangle order

black 255 whiteout                     \ black ink, white background
grayscale_image                        \ we're making a gray scale image
                                       \ do we set a pixel or not?
: ?pixel over over and if drop drop else set_pixel then ;
: triangle 1 order lshift dup 0 do dup 0 do i j ?pixel loop loop drop ;

triangle s" triangle.ppm" save_image   \ done, save the image