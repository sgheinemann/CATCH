  ;+
  ;******************************************************************************************
  ; NAME: extract_ch
  ; 
  ; Call: 
  ;       extract_ch, in_mp, result_map, minval, maxval, inverse=inverse, write_fits = write_fits,
  ;                   outpath=outpath, sun=sun, only_inner = only_inner, retval=retval, Nan=NaN, 
  ;                   background=background, silent = silent
  ;        
  ; PURPOSE: 
  ;        Search for structures lying in a given intensity range.
  ;        Extract these structures and return them in a list of structures.
  ;        (These is done by a recursive algorithm. If one pixel is found, it looks at neighboring 
  ;        pixels. If this neighboring pixel is also within intensity range, the algorithm is once
  ;        again called with the found pixel as new starting pixel.)
  ;        
  ; Limitations:
  ;       Only Structures with equal or more 2x2 pixels are extracted.
  ;        
  ; INPUTS:  
  ;       in_mp            Image in which to search. Can be a map or a path to one fits-file.
  ;       minval           Intensity range minimum.
  ;       maxval           Intensity range maximum.
  ;       inverse          Search for structures with intensities out of intensity range 
  ;                        instead of intensities in intensity range.
  ;       write_fits       Save found structures in fits-files.
  ;       outpath          Directory to save found Structures.
  ;       sun              Removes limb of sun
  ;       only_inner       Find only structures which don't touch the limb of sun. Only with sun.
  ;       retval           If this value is in one found structure, this structure is not returned.
  ;       NaN              Ignore NaNs.
  ;       background       Background value of output image.
  ;       mask             defines a searching mask for neighboring pixels. Standard is [[1, 1], [1, 0], [1, -1], [0, 1], [0, -1], [-1, 1], [-1, 0], [-1, -1]]
  ;       silent           no progress messages
  ;
  ; OUTPUTS:
  ;       result_map       Array of extracted structures.
  ;       
  ; Execution time: ~ 1 second foreach 7000 pixels found (tested on igam17ws on 4 Oct, 2016)
  ;
  ; MODIFICATION HISTORY:
  ;        Written by: Stefan Hofmeister, Juny 11, 2014
  ;
  ;        Modified: 
  ;                    4 Oct 2016     changed get_pix output from array to list -> increased execution speed (Stefan Hofmeister)
  ;*******************************************************************************************

pro extract_ch, in_mp, result_map, minval, maxval, inverse=inverse, write_fits=write_fits, outpath=outpath, only_inner = only_inner, sun = sun, retval = retval, NaN = NaN, background = background, mask = mask, silent = silent

;// Load file/map
if keyword_set(background) eq 0 then background = !values.f_nan
if keyword_set(inverse) eq 0 then inverse = 0
filename = ''
if isa(in_mp, 'string') then begin
  if file_exist(in_mp) then begin
    filename_std = in_mp
    read_sdo, in_mp, index, dat
    dat = double(dat)
    index2map, index, dat, in_mp
  endif else begin
    print, 'extract_ch: file not found'
    return
  endelse 
endif else begin
  if isa(in_mp.data, "float") eq 0 and isa(in_mp.data, "double") eq 0 then begin
    print, "extract_ch: converting datatype of image into double" 
    map2index, in_mp, index, data
    index2map, index, double(data), in_mp
  endif
endelse

im = in_mp.data
size_tmp = size(im, /dimensions)
dim_x = size_tmp[0]
dim_y = size_tmp[1]
;// if sun: remove limb; if only_inner: set limb to retval
if keyword_set(only_inner) then retval = 1.23456789  
if keyword_set(sun) then begin
  rad_px = pb0r(in_mp.time, /arcsec)
  rad_px = floor(rad_px[2] / in_mp.dx)
  for i = 0, dim_x -1 do begin
  for j = 0, dim_y -1 do begin
    if (sqrt((i-dim_x/2)^2 + (j-dim_y/2)^2) gt rad_px) then begin
      if keyword_set(only_inner) then im[i, j] = retval else im[i, j] = !Values.D_NAN
    endif
  endfor
  endfor
endif

;// if requested: set NaN's to minval - only values gt / lt minval will be returned ;)
if keyword_set(NaN) then im[where(finite(im, /nan) eq 1)] = minval
im = double(im)

; Search for coronal holes -> get_px

if exist(retval) eq 1 then save_retval = retval
result = list()
for i = 0, dim_x -1 do begin  ;start at each image position
  for j = 0, dim_y -1 do begin
    if ((im[i, j] lt maxval && im[i, j] gt minval && inverse eq 0 ) || ((im[i, j] gt maxval || im[i, j] lt minval) && inverse eq 1 )) then begin ;if intensity inside intensity range is found:
      if exist(retval) eq 1 then if im[i, j] eq retval then continue  ;check if intensity is retval
      undefine, data

      if keyword_set(silent) eq 0 then print,  'extract_ch: working...'
      stat = 0
      get_px, im, i, j, minval, maxval, inverse, data, retval=retval, stat = stat, mask = mask  ;get array of positions of found structure pixels; is a recursive program ;)
      if stat eq 1 then continue
      data_array = data.toarray(/transpose)
      result.add, data_array  ;add found structure to found list
     endif
  endfor
endfor 

;Combine positions of found structure pixels to images
result_map = list()
z=0
foreach ch, result do begin  ;for each found structure
  
  ;defines frame of image
  max_x = max(ch[0,*])
  min_x = min(ch[0,*])
  max_y = max(ch[1,*])
  min_y = min(ch[1,*])
  
  ;create an empty image
  msk_ch = make_array(max_x - min_x +1, max_y - min_y +1, /double, value = background)
  
  ;if image is not 2-dimensional: bargage image
  if (n_dimensions(msk_ch) lt 2) || where(size(msk_ch, /dimensions) eq 1) ne -1 then continue
  
  ; create image: copy values of found image positions from input image to output image; result is 2d-array
  for i= 0, n_elements(ch[0,*]) -1 do msk_ch[ch[0,i] - min_x, ch[1,i] - min_y] = in_mp.data[ch[0,i], ch[1,i]]
  
  ;create submap -> =get correct header information for new map
  sub_map, in_mp, smap, xrange=[min_x, max_x], yrange=[min_y, max_y], /pixel, /noplot
  smap.data = msk_ch ;set image as image of new map
  
  result_map.add, smap ;// done: add to result
  
  ;if requested: save maps
  if keyword_set(write_fits) then begin
    map2index, smap, index, data
    if not exist(outpath) then outpath = './'
    if (filename_std ne '') then filename = file_basename(filename_std, 'fits') + strtrim(string(z), 2) + '.fits' $
    else filename = anytim(index.date_obs, /ccsds) + '.' + strtrim(string(z), 2) + '.fits'
    z++
    mwritefits, index, data, outfile = outpath + filename
  endif
endforeach

if keyword_set(silent) eq 0 then print,  'extract_ch: ' + strtrim(string(n_elements(result)), 2) + ' structures extracted. Done.'

end
;
;
pro get_px, im, xc, yc, minval, maxval, inverse, data, retval = retval, stat = stat, mask = mask  ;im: 2dim-array Bild, xc, y: Startposition exctract, value: Wert, der extrahiert werden soll (=Farbe des Gebildes), nvalue: Wert, der nicht exctrahiert werden soll, data: Rückgabearray der exctrahierten Positionen
;Search for pixels which border on found pixel and lies within intensity range. If such a pixel is found, save pixel position, delete pixel (<- already found), set found pixel as start pixel and call get_px once again. -> recursivly search for bordering pixels within intensity range.

;Pixel was already found one program level higher. So do:
if (((im[xc, yc] gt minval) && (im[xc, yc] lt maxval) && (inverse eq 0) ) || ((im[xc, yc] gt maxval) || (im[xc, yc] lt minval) && (inverse eq 1 ))) then im[xc,yc]= minval else return ;value: delete pixel -> important for exit condition(!)
if exist(data) eq 0 then data = list()
data.add, [xc,yc]; add found pixel

if keyword_set(mask) eq 0 then mask = [[1,0], [-1, 0], [0, 1], [0, -1], [1, 1], [1, -1], [-1, 1], [-1, -1]]
pos = mask
for i = 0, n_elements(mask[0,*]) -1 do begin ;search for bordering pixels. pos defines the direction in which will be searched. 
   if xc+pos[0, i] lt 0 || xc+pos[0, i] gt n_elements(im[*,0]) -1 || yc+pos[1, i] lt 0 || yc+pos[1, i] gt n_elements(im[0,*]) -1 then continue ;if pixel we are looking at is outside of image: return one program level higher
   if keyword_set(retval) then if im[xc+pos[0, i], yc+pos[1,i]] eq retval then begin ;if retval was found: go to first level of program (if stat is once set, stat will also be 1 set at each higher program level <- call by reference(!))
    stat = 1 ;set flag
    return
  endif 
   ;if pixel we are looking at is within intensity range: call get_px once again with this pixel as new starting pixel
   if (im[xc+pos[0, i], yc+pos[1,i]] lt maxval && im[xc+pos[0, i], yc+pos[1,i]] gt minval && inverse eq 0 ) || ((im[xc+pos[0, i], yc+pos[1,i]] gt maxval || im[xc+pos[0, i], yc+pos[1,i]] lt minval) && inverse eq 1 ) then get_px, im, xc+pos[0,i], yc+pos[1,i], minval, maxval, inverse, data, retval = retval, stat = stat, mask = mask
endfor
end

