pro calc_masscenter, inmap, masscenter,coord=coord, observer=observer, coreg = coreg;masscenter360=masscenter360,

  ;+
  ; NAME:
  ;    calc_masscenter
  ;
  ; PURPOSE:
  ;   calculates the center of mass of a coronal hole, coordinates are in HEEQ
  ;   ( [0,0] is the position of the HEEQ at this time, im relation to the observer )
  ;
  ;
  ; CALLING SEQUENCE:
  ;  calc_masscenter, inmap, masscenter, observer=observer
  ;
  ;
  ; INPUTS:
  ;  map ... area, from where the center of mass shall be calculated
  ;
  ;
  ; OUPUTS:
  ;  masscenter ... center of mass in [ latitude, longitude ]
  ;
  ;
  ; KEYWORDS:
  ;       observer ... set type of observer: 'STEREO_A', 'STEREO_B'
  ;                    Default: 'SDO'
  ;
  ;       coreg ... correction array (for radial and area correction)
  ;
  ;       coord ... helioprojective coordinates (arcsec) of the center of mass in this image
  ;
  ;       masscenter360 .. returns the HEEQ Cordinates in 360 Degree Mode
  ;
  ; MODIFICATION HISTORY:
  ;   Written by: Stephan Heinemann, 02-09-16
  ;   Based on: Code by Stefan Hofmeister
  ;
  ;   Modified by: Stephan Heinemann, 17-11-16
  ;                Coreg Keyword was made optional
  ;
  ;   Modified by: Stephan Heinemann, 22-12-16
  ;                Correction was applied to SDO input to have all coordinates in the correct system ('HEEQ')
  ;
  ;   Modified by: Stephan Heinemann, 24-07-19
  ;                If an unknown observer (=/= SDO, STEREO_A, STEREO_B) is forwarded then EARTH is assumed
  ;-

  map=inmap

  map2index, map, index, data
  ;if keyword_set(coreg) eq 1 then begin
  ;tmpcoreg=coreg
  ;endif else begin
  ;  tmpcoreg=1
  ;endelse
  tmpcoreg=fcheck(coreg,1)
  
  map.data[*]=!values.f_nan
  map.data[where(finite(inmap.data) eq 1)]=1  ; map should be binary!

  date=index.date_obs
  
  
if keyword_set(observer) eq 0 then begin
  ang=pb0r(date, /earth, l0=l0,/arcsec)
endif
if  keyword_set(observer) eq 1 then begin
  if observer eq 'SDO' then begin
      ang=pb0r(date, /earth, l0=l0,/arcsec)
  endif else begin
    if observer eq 'STEREO_A' then begin
      ang=pb0r(date, STEREO='A', l0=l0,/arcsec)
    endif else begin
      if observer eq 'STEREO_B' then begin
        ang=pb0r(date, STEREO='B', l0=l0,/arcsec)
      endif else begin
        ang=pb0r(date, /earth, l0=l0,/arcsec)
      endelse
    endelse
  endelse
endif
  p=ang[0]
  b0=ang[1]
  r=ang[2]
  
  schwerpunkt_px = fltarr(2)
  pxdatacoreg = tmpcoreg*map.data
  for i = 1, n_elements(map.data[*,0]) do schwerpunkt_px[0] = schwerpunkt_px[0] + total(pxdatacoreg[i-1, *], /nan) * i
  for i = 1, n_elements(map.data[0,*]) do schwerpunkt_px[1] = schwerpunkt_px[1] + total(pxdatacoreg[*, i-1], /nan) * i
  schwerpunkt_px = schwerpunkt_px/total(pxdatacoreg, /nan)
  schwerpunkt_coord = [ ( schwerpunkt_px[0] - (index.crpix1)) * index.cdelt1 , (schwerpunkt_px[1] - (index.crpix2 )) * index.cdelt2 ]

  
  
  schwerpunkt_h = arcmin2hel(schwerpunkt_coord[0]/60, schwerpunkt_coord[1]/60, date=date, b0=b0,l0=l0,rsun=r,p=p)
  

  ;masscenter360=schwerpunkt_360
  masscenter=schwerpunkt_h
  coord=schwerpunkt_coord

  return
end
