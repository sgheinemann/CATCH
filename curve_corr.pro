pro curve_corr, inmap, corr_map, ref_map=ref_map, limb=limb, $
                               area=area, flux=flux, total_flux=total_flux, coreg=coreg,$
                               mag1=mag1, mag2=mag2, corr_area=corr_area

;+
; NAME:
;    curve_corr
;
; PURPOSE:
;   calculates a correction of the curvature
;   
;
; CALLING SEQUENCE:
;     curve_corr, inmap, corr_map, ref_map=ref_map, limb=limb, observer=observer, $
;                              area=area, flux=flux, total_flux=total_flux, 
;  
;
; INPUTS:
;  map ... map from whom the binary map will be extracted
;
; OUPUTS:
;  corr_map ...curvature corrected map
; 
;
; KEYWORDS:
;       ref_map ... reference map
;       
;       limb ... if set, off-limp pixel are set to NaN
;         
;       area ... if set, saves the total area into the area variable, area in km^2
;         
;       flux ... if set, saves the total open flux into the flux variable
;                           
;       total_flux ... if set, saves the total flux into the total_flux variable
;       
;       corr ... if set, saves tmpcoreg in the corr variable
;       
;       area_corr ... returns area corrected map
;	
;	mag1/2 ... are for testing only
;                           
; MODIFICATION HISTORY:
;   Written by: Stephan Heinemann, 02-09-16
;   Based on: Code by Stefan Hofmeister
;   
;   Modified by: Stephan Heinemann, 13-12-16
;                Removed the observer keyword and added the automatic extraction of the solar radius from the map
;                
;   Modified by: Stephan Heinemann, 07-03-19
;               removed map2index to avoid spamming of box_messages during long loops            
;-
map=inmap
if keyword_set(ref_map) eq 1 then begin
  sub_map, map, newmap, ref_map=ref_map 
  ;map2index, newmap, newindex, newdata
  newmap.data[where( ref_map.data ne 1 )] =!values.f_nan
  
endif else begin
  newmap=map
  ;map2index, newmap, newindex, newdata
endelse
crpix1=comp_fits_crpix(newmap.xc,newmap.dx,n_elements(newmap.data[*,0]),0.)
crpix2=comp_fits_crpix(newmap.yc,newmap.dy,n_elements(newmap.data[0,*]),0.)



if keyword_set(limb) eq 1 then newmap=rem_map_limb(newmap, missing = !values.f_nan)


    date=anytim(newmap.time,/ccsds)
    tmpcoreg = newmap.data 

    tmpcoreg[where(finite(tmpcoreg) eq 1)] = 1
    tmparea = tmpcoreg
  
;if keyword_set(observer) eq 1 then begin
;  if observer eq 'STEREO_A' then Sonnenradius = pb0r(date, /arcsec,stereo='A')
;  if observer eq 'STEREO_B' then Sonnenradius = pb0r(date, /arcsec,stereo='B')
;  if observer eq 'SDO' then Sonnenradius = pb0r(date, /arcsec)
;  endif else begin
;      Sonnenradius = pb0r(date, /arcsec)
;  endelse
;    Sonnenradius = Sonnenradius[2]

    angles=get_map_angles(map)
    Sonnenradius=angles.rsun
    
    arcsecTokm = 1/Sonnenradius*696342 ; in km
    xsize = n_elements(tmparea[*,0])
    ysize = n_elements(tmparea[0,*])
      for i = 0, n_elements(tmparea[*,0]) - 1 do begin
        for j = 0, n_elements(tmparea[0, *]) -1 do begin
          DistanzSonnenmitte = sqrt(((i - crpix1)*newmap.dx)^2 + ((j - crpix2)*newmap.dy)^2)
          WinkelSonnenmitteMittelpunktArea = asin(DistanzSonnenmitte / Sonnenradius)
          tmpcoreg[i, j] = 1/cos(WinkelSonnenmitteMittelpunktArea) 
        endfor
      endfor
   
   
    tmparea = float(tmparea) * tmpcoreg* newmap.dx* newmap.dy* arcsecTokm^2         ; in km2 
    newmap.data=newmap.data*tmpcoreg                                                    ;correction because of the projected area
    magFlusscoreg =  tmpcoreg * newmap.dx* newmap.dy* arcsecTokm^2 *1e10  ;correction of the magnetic flux because of the radial magnetic field
    flux_px=magFlusscoreg*newmap.data
    corr_area=tmparea
    
    mag_mom=moment(newmap.data,/NaN,maxmoment=1)
    mag1=mag_mom[0]
    area=total(tmparea,/NaN)
    corr_map=newmap
    flux=total(flux_px,/NaN)
    total_flux=total(abs(flux_px),/NaN)
    
    mag2=flux/area
    
    coreg=tmpcoreg

  return
  end
