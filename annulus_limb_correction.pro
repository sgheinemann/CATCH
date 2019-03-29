function annulus_limb_correction, map
; applies the annulus limb-brightening correction for euv-images taken by AIA or EUVI. See the Verbeek et al (2014): The SPoCA-suite: Software for extraction, characterization, and tracking of active regions and coronal holes on EUV images

;calculate distance of the pixels to the center of the Sun in %R_sun
siz = n_elements(map.data[*, 0])
xarr = fltarr(siz, siz)
yarr = fltarr(siz, siz)
fillx = (indgen(siz) -siz/2.)* map.dx  + map.xc
filly = (indgen(siz) -siz/2.)* map.dy  + map.yc
for i = 0, siz-1 do xarr[*, i] = fillx
for i = 0, siz-1 do yarr[i, *] = filly
distarr = sqrt(xarr*xarr + yarr*yarr) / map.rsun
alc = [0.70, 0.95, 1.08, 1.12]

;calculate median of inner shell
median_inner = median(map.data[where(distarr lt alc[0])])

;correct middle-inner shell
for i = alc[0], alc[1], 0.01 do begin
  ind = where(distarr ge i and distarr lt i + 0.01)
  median_shell = median(map.data[ind])
  alc1 = 0.5 * sin(!pi/(alc[1] - alc[0]) * (i - (alc[1] + alc[0])/2.)) + 0.5
  corr = (1 - alc1) * map.data[ind] + alc1 * median_inner * map.data[ind] / median_shell
  map.data[ind] = corr
  median_shell = median(map.data[ind])
endfor

;correct middle-outer shell
for i = alc[1], alc[2], 0.01 do begin
  ind = where(distarr ge i and distarr lt i + 0.01)
  median_shell = median(map.data[ind])
  map.data[ind] = median_inner * map.data[ind] / median_shell
  median_shell = median(map.data[ind])
endfor

;correct outer shell
for i = alc[2], alc[3], 0.01 do begin
  ind = where(distarr ge i and distarr lt i + 0.01)
  median_shell = median(map.data[ind])
  alc2 =  0.5 * sin(!pi/(alc[3] - alc[2]) * (i - (alc[3] - alc[2])/2.)) + 0.5
  map.data[ind] = (1 - alc2) * map.data[ind] + alc2 * median_inner * map.data[ind] / median_shell
endfor


return, map

end
