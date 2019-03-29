;########################### Thr extraction Widget ####################################
pro ex_widget, ev

  common general, id, paths,dir,debug
  common menu_id, menuid
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  common insitu, insi_ids, date_map, date_plasma, speed, density, temp, date_mag, btsc, bx,by,bz,ranges
  common dtool, boundary_exists, draw_binmap,dcord2,xcord2,ycord2,draw_dex,mousebutton 
  
  widget_control, id.main, TLB_GET_OFFSET=offs
  
  ex_main = widget_base(title='CATCH: CH Extraction', xsize=1000, ysize=700,SCR_XSIZE=1000,SCR_YSIZE=700,modal=(1-debug), group_leader=id.main, xoffset=offs[0]-170, yoffset=offs[1]-205)
  
  extracted =0
  boundary_exists=0
  
  top = Widget_Base(ex_main, XOFFSET=10)
  label = Widget_Label(top, value='Coronal hole extraction algorithm based on median solar disk intensity threshold', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  extr = Widget_Base(top, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=680,xsize=980)
  label = Widget_Label(top, value='Coronal hole extraction algorithm based on median solar disk intensity threshold', XOFFSET=10)
  
  draww = widget_draw(extr, uvalue='draw',/MOTION_EVENTS,xsize=512, ysize=512, xoffset=980-522, yoffset=10,/frame,/button)
  
  top2 = Widget_Base(extr, XOFFSET=10, yoffset=10)
  label = Widget_Label(top2, value='File Management', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  data_field = Widget_Base(top2, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=265,xsize=428)
  label = Widget_Label(top2, value='File Management', XOFFSET=10)
  
  sv_path=widget_button(data_field,value='Output Directory:',uval='sv_path', /no_release, xsize=130, xoffset=10, yoffset=20,ysize=25, tooltip='Specify output directory for results')
  sv_path_text=widget_TEXT(data_field,value=paths.outpath, xsize=42, XOFFSET=150,yoffset=17, ysize=1, uvaL='sv_text', units=0,/editable)
  
  ld_path=widget_button(data_field,value='Data Directory:',uval='ld_path', /no_release, xsize=130, xoffset=10, yoffset=55,ysize=25, tooltip='Specify input directory for CH extraction')
  ld_path_text=widget_TEXT(data_field,value=paths.euvpath, xsize=42, XOFFSET=150,yoffset=52, ysize=1, uvaL='ld_text', units=0,/editable)
  
  label_list = WIDGET_LABEL(data_field, XSIZE=100, VALUE='Files:', xoffset=10, yoffset=100, /align_left)
  refresh_list=widget_button(data_field,value='Refresh',uval='refresh', /no_release, xsize=100, xoffset=120, yoffset=95,ysize=25, tooltip='Refresh list of input files')
  
  temp_path=paths.euvpath
  lfiles=file_search([paths.euvpath+'*.fits',paths.euvpath+'*.fts',paths.euvpath+'*.sav',paths.euvpath+'efz*'])
  lfiles=file_basename(lfiles)
  files_list=widget_list(data_field,value=lfiles,uval='files', xsize=63, xoffset=10, yoffset=130,ysize=4)
  
  load=widget_button(data_field,value='Load', uval='load',/NO_release, xoffset=318, yoffset=230,xsize=100,ysize=25,tooltip='Load selected file')
  if paths.lbc eq 'on' then begin
  lbc=widget_button(data_field,value='LBC on', uval='lbc',/NO_release, xoffset=10, yoffset=230,xsize=100,ysize=25,tooltip='Turn Limb Brightening Correction on/off')  
  endif else begin
  lbc=widget_button(data_field,value='LBC off', uval='lbc',/NO_release, xoffset=10, yoffset=230,xsize=100,ysize=25,tooltip='Turn Limb Brightening Correction on/off')
  endelse
  
  label_reso = WIDGET_LABEL(data_field, XSIZE=70, VALUE='Resolution', yoffset=235, xoffset=130, /align_left)
  cbox_reso=widget_combobox(data_field,value=['4096x4096','2048x2048','1024x1024','512x512','256x256'], uval='cbox_reso', xsize= 100, yoffset=230, xoffset=200,ysize=25)  
  case paths.res_euv of
    256:COMBOBOX_def=0
    512:COMBOBOX_def=1
    1024:COMBOBOX_def=2
    2048:COMBOBOX_def=3
    4096:COMBOBOX_def=4
  endcase
  widget_control, cbox_reso, set_combobox_select=COMBOBOX_def 
    
  top3 = Widget_Base(extr, XOFFSET=10, yoffset=295)
  label = Widget_Label(top3, value='Extraction Parameters', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  input_field = Widget_Base(top3, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=217,xsize=428)
  label = Widget_Label(top3, value='Extraction Parameters', XOFFSET=10)
  
  thresh=widget_slider(input_field, title='           Threshold [%]', uval='thresh', min=15., max=100., value=35., xsize=200, xoffset=10, yoffset=10,/align_center )  ;threshold
  morph=widget_slider(input_field,  title='           Morph Radius', uval='morph', min=0., max=15., value=2., xsize=200, xoffset=218, yoffset=10 )
  
  draw_hist = widget_draw(input_field, uvalue='draw_hist', xsize=408, ysize=105, xoffset=10, yoffset=67,/frame)
  
  activate=widget_button(input_field,value='Apply Extraction', uval='appl',/NO_release, xoffset=10, yoffset=182,xsize=408,ysize=25,$
              tooltip='Apply the extraction algorithm with the specified threshold and morph radius to the input data, then select coronal hole')
  
  
  abort= widget_button(extr,value='Done', uval='abort',/NO_release, xsize=110 , xoffset=740., yoffset=645, tooltip='Exit the extraction widget',ysize=25)
  exit= widget_button(extr,value='Exit', uval='exit',/NO_release, xsize=110 , xoffset=860., yoffset=645, tooltip='Exit CATCH',ysize=25)
  sav= widget_button(extr,value='Save', uval='save',/NO_release, xsize=262 , xoffset=980-512., yoffset=645, tooltip='Save the extracted parameters to the output directory',ysize=25)
  insitu= widget_button(extr,value='In-Situ Data', uval='insitu',/NO_release, xsize=126 , xoffset=980-512.+136., yoffset=585, tooltip='View In-Situ data',ysize=25)
  
  popt= widget_button(extr,value='Plot Options', uval='poptions',/NO_release, xsize=126 , xoffset=980-512, yoffset=585, tooltip='Configure plot',ysize=25) ; image scaling, grid_spacing (on/off)
  
  insitu_predi= widget_button(extr,value='In-Situ Prediction', uval='insitu_predi',/NO_release, xsize=126 , xoffset=980-512.+136., yoffset=615, tooltip='Calculate HSS prediction profile',ysize=25)
  draw_tool= widget_button(extr,value='Drawing Tool', uval='draw_tool',/NO_release, xsize=126 , xoffset=980-512, yoffset=615, tooltip='Configure coronal hole mask',ysize=25)
  
  
    interm_res = Widget_Base(extr, XOFFSET=10, yoffset=532)
    label = Widget_Label(interm_res, value='Coronal Hole Properties', XOFFSET=10)
    labelGeometry = Widget_Info(label, /GEOMETRY)
    labelYSize =  labelGeometry.ysize
    results = Widget_Base(interm_res, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=128,xsize=428)
    label = Widget_Label(interm_res, value='Coronal Hole Properties', XOFFSET=10)
  
    label_area = WIDGET_LABEL(results, XSIZE=170, VALUE=   'Area [10^10 km^2]         :', xoffset=10, yoffset=15,/ALIGN_LEFT)
    label_medint = WIDGET_LABEL(results, XSIZE=170, VALUE= 'Median Intensity [DN]     :', xoffset=10, yoffset=30,/ALIGN_LEFT)
    label_meanint = WIDGET_LABEL(results, XSIZE=170, VALUE='Mean Intensity [DN]       :', xoffset=10, yoffset=45,/ALIGN_LEFT)
    label_lng = WIDGET_LABEL(results, XSIZE=170, VALUE=    'CoM Longitude [Deg]       :', xoffset=10, yoffset=60,/ALIGN_LEFT)
    label_lat = WIDGET_LABEL(results, XSIZE=170, VALUE=    'CoM Latitude [Deg]        :', xoffset=10, yoffset=75,/ALIGN_LEFT)
    label_lngex = WIDGET_LABEL(results, XSIZE=170, VALUE=  'Longitudinal Extent [Deg] :', xoffset=10, yoffset=90,/ALIGN_LEFT)
    label_latex = WIDGET_LABEL(results, XSIZE=170, VALUE=  'Latitudinal Extent [Deg]  :', xoffset=10, yoffset=105,/ALIGN_LEFT)
  
    vlabel_area = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=15)
    vlabel_medint = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=30)
    vlabel_meanint = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=45)
    vlabel_lng = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=60)
    vlabel_lat = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=75)
    vlabel_exlng = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=90)
    vlabel_exlat = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=170, yoffset=105)
    
    vlabel_area_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=15)
    vlabel_medint_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=30)
    vlabel_meanint_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=45)
    vlabel_lng_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=60)
    vlabel_lat_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=75)
    vlabel_exlng_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=90)
    vlabel_exlat_pm = WIDGET_LABEL(results, XSIZE=20, VALUE=string(177b), xoffset=280, yoffset=105)
    
    vlabel_area_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=15)
    vlabel_medint_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=30)
    vlabel_meanint_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=45)
    vlabel_lng_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=60)
    vlabel_lat_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=75)
    vlabel_exlng_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=90)
    vlabel_exlat_err = WIDGET_LABEL(results, XSIZE=110, VALUE=' ', xoffset=290, yoffset=105)
    
    traffic_lights = widget_draw(results, uvalue='traffic_lights', xsize=16, ysize=16, xoffset=400, yoffset=16, RENDERER=1)
    
    label1 = WIDGET_LABEL(extr, XSIZE=262, VALUE='X position:', xoffset=980-522, yoffset=522+10)
    label2 = WIDGET_LABEL(extr, XSIZE=262, VALUE='Y position:', xoffset=980-522, yoffset=522+25)
    label3 = WIDGET_LABEL(extr, XSIZE=262,  VALUE='Current Intensity [DN]:', xoffset=980-522, yoffset=522+40)
   
;    label3 = WIDGET_LABEL(extr, XSIZE=512,  VALUE='Selected X position:', xoffset=980-522, yoffset=522+60)
;    label4 = WIDGET_LABEL(extr, XSIZE=512, VALUE='Selected Y position:', xoffset=980-522, yoffset=522+75)
;    label6 = WIDGET_LABEL(extr, XSIZE=512, VALUE='Selected Intensity [DN]:', xoffset=980-522, yoffset=522+90)
    
    widget_control, insitu,sensitive=0
    widget_control, sav,sensitive=0
    widget_control, activate,sensitive=0
    widget_control, draw_hist,sensitive=0
    widget_control, draww,sensitive=0
    widget_control, morph,sensitive=0
    widget_control, thresh,sensitive=0
    widget_control, popt, sensitive=0
    widget_control, traffic_lights, sensitive=0
    widget_control, draw_tool, sensitive=0
    widget_control, insitu_predi, sensitive=0
    
    widget_control,/realize,ex_main
    WIDGET_CONTROL, traffic_lights, GET_VALUE=logoID_traffic_lights
    
    WSET, logoID_traffic_lights
    
    graycol=194
    lights=make_array(3,16,16) & lights[0,*,*]=graycol & lights[1,*,*]=graycol & lights[2,*,*]=graycol
    tvimage, lights
    
    WIDGET_CONTROL, draww, GET_VALUE=drawID
    WIDGET_CONTROL, draw_hist, GET_VALUE=draw_histID
  
    logo_draw_catch = widget_draw(extr, uvalue='logo_catch', xsize=230, ysize=98, yoffset=542, xoffset=740)

    undefine, plot_scl
    
    WIDGET_CONTROL, logo_draw_catch, GET_VALUE=logoID_catch
    WSET, logoID_catch

    logo_path=dir+'catch_logo.jpg'
    read_jpeg,logo_path, logo
    logo=congrid(logo,3,230*2,98*2,/center)
    tvimage, logo
    widget_control, logo_draw_catch, sensitive=0
   
    
    ids={insitu:insitu,save:sav,abort:abort,exit:exit,activate:activate, draw_hist:draw_hist,draww:draww,morph:morph,draw_tool:draw_tool,$
         thresh:thresh, label1:label1,label2:label2,label3:label3, vlabel_area:vlabel_area,vlabel_medint:vlabel_medint,insitu_predi:insitu_predi,$
         vlabel_meanint:vlabel_meanint,vlabel_lng:vlabel_lng,vlabel_lat:vlabel_lat, vlabel_exlng:vlabel_exlng,vlabel_exlat:vlabel_exlat,$
         vlabel_area_err:vlabel_area_err,vlabel_medint_err:vlabel_medint_err, vlabel_meanint_err:vlabel_meanint_err,logoID_traffic_lights:logoID_traffic_lights,$
         vlabel_lng_err:vlabel_lng_err, vlabel_lat_err:vlabel_lat_err, vlabel_exlng_err:vlabel_exlng_err,vlabel_exlat_err:vlabel_exlat_err,$
         ex_main:ex_main,lbc:lbc,cbox_reso:cbox_reso, files_list:files_list,drawID:drawID, draw_histID:draw_histID, ld_path:ld_path_text,thick_text:long(0),$
         sv_path:sv_path_text, opt_main:long(0),dmin_text:long(0), grid_text:long(0),dmax_text:long(0), apply_opt:long(0),abort_opt:long(0), popt:popt,$
         draw_main:long(0), apply_draw:long(0), abort_draw:long(0), draw_big:long(0), draw_radius:long(0), popt_draw:long(0), new_draw:long(0), draw_morph:long(0),$
         draw_morph_activ:long(0), draw_erase:long(0),draw_bigID:long(0) }
        
       
  xmanager, 'ex',ex_main,  /no_block
 

end


PRO ex_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  common insitu, insi_ids, date_map, date_plasma, speed, density, temp, date_mag, btsc, bx,by,bz,ranges
  common version, version
  common dtool, boundary_exists, draw_binmap,dcord2,xcord2,ycord2,draw_dex,mousebutton
  
  widget_control, ev.id, get_uvalue=uvalue                        ; get the uvalue
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN
    if extracted  eq 1 then begin
    IF (ev.press eq 1) THEN BEGIN
      widget_control,/hourglass
      Dx = (double(ev.x) / dcord.X_VSIZE - xcord.s[0]) / xcord.s[1]
      DY = (double(ev.Y) / dcord.y_VSIZE - Ycord.s[0]) / ycord.s[1]
      crd=[Dx,Dy]
      p_sel=[crd[0]/windex.cdelt1+windex.crpix1,crd[1]/windex.cdelt2+windex.crpix2]
      
      if bmaps[0].data[p_sel[0],p_sel[1]] gt 0 and bmaps[1].data[p_sel[0],p_sel[1]] gt 0 and bmaps[2].data[p_sel[0],p_sel[1]] gt 0 and $
        bmaps[3].data[p_sel[0],p_sel[1]] gt 0 and bmaps[4].data[p_sel[0],p_sel[1]] gt 0 then begin
       
      chmaps=bmaps
      seed_dex=fix(p_sel[0])+fix(p_sel[1])*n_elements(chmaps[0].data[*,0])
      
      ch_prop=make_array(7,11); binmaps1-5, average, stddev;area, meanint, medianint, com_lng, com_lat, com_xc, com_yc, lng_min, lng_max, lat_min, lat_max

      
      for i=0, 4 do begin
        roi=region_grow(bmaps[i].data, seed_dex, /all_neighbors, threshold=[0.5,1.5])
        chmaps[i].data[*]=0  &  chmaps[i].data[roi]=1
        helpmap=chmaps[i]    &  helpmap.data[where(chmaps[i].data eq 0)]=!values.f_nan  
      curve_corr, helpmap, corr_map,coreg=coreg,area=area
      
      if map_identifyer eq 'SOHO' then begin & mc_obs='SOHO' & endif else begin
        if map_identifyer eq 'SDO'  then begin & mc_obs='SDO' & endif else begin
          if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then mc_obs='STEREO_A'
          if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then mc_obs='STEREO_B'
        endelse
      endelse
      
      calc_masscenter, corr_map, xy_center,coord=coord, coreg = coreg, observer=mc_obs
      
      ch_prop[i,0]=area
      ch_prop[i,1]=median(workmap.data[where(chmaps[i].data eq 1)])
      meanint_mom=(moment(workmap.data[where(chmaps[i].data eq 1)],/nan,maxmoment=2))
      ch_prop[i,2]=meanint_mom[0]
      ch_prop[i,3]=xy_center[1] &  ch_prop[i,4]=xy_center[0]
      ch_prop[i,5]=coord[0] & ch_prop[i,6]=coord[1]
      
      mcrpix1=comp_fits_crpix(chmaps[i].xc,chmaps[i].dx,n_elements(chmaps[i].data[*,0]),0.)
      mcrpix2=comp_fits_crpix(chmaps[i].yc,chmaps[i].dy,n_elements(chmaps[i].data[0,*]),0.)
      mapdate=anytim(chmaps[i].time,/ccsds)

      ind = ARRAY_INDICES(chmaps[i].data, where(chmaps[i].data eq 1))
      ind_coord = [ ( ind[0,*] - (mcrpix1)) * chmaps[i].dx , (ind[1,*] - (mcrpix2 )) * chmaps[i].dy ]
      if map_identifyer eq 'SOHO' then begin & ang=pb0r(workmap.time, /soho, l0=l0,/arcsec) & endif else begin
      if map_identifyer eq 'SDO'  then begin & ang=pb0r(workmap.time, /earth, l0=l0,/arcsec) & endif else begin
      if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then ang=pb0r(workmap.time, /sta, l0=l0,/arcsec)
      if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then ang=pb0r(workmap.time, /stb, l0=l0,/arcsec)
        endelse
      endelse
      
      p=ang[0] & b0=ang[1] & r=ang[2]
      lonlat_coord = arcmin2hel(ind_coord[0,*]/60, ind_coord[1,*]/60, date=mapdate, b0=b0,l0=l0,rsun=r,p=p)

      minmax_lng_px=minmax(lonlat_coord[1,*],/nan)
      minmax_lat_px=minmax(lonlat_coord[0,*],/nan)

      ch_prop[i,7]=minmax_lng_px[0] &    ch_prop[i,8]=minmax_lng_px[1]
      ch_prop[i,9]=minmax_lat_px[0] &    ch_prop[i,10]=minmax_lat_px[1]
      endfor
       
       for i=0, 10 do begin
       help_var=moment(ch_prop[0:4,i],/nan, maxmoment=2)
       ch_prop[5,i]=help_var[0] & ch_prop[6,i]=max(sqrt((ch_prop[0:4,i]-ch_prop[5,i])^2))
       endfor
   
       WSET, ids.drawID

       pmap=themap
      case map_identifyer of
        'SOHO': eit_colors, 195,/silent
        'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
        'SDO': aia_lct, wave='193', /load
      endcase
      
             
       ixixdate=anytim(themap.time,/ccsds)
       
       plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255 
       plot_map, chmaps[0], /over, /cont, color=cgcolor('blue'),c_thick=plot_scl[2], c_linestyle=0
       plot_map, chmaps[4], /over, /cont, color=cgcolor('blue'),c_thick=plot_scl[2], c_linestyle=0
       plot_map, chmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[2], c_linestyle=0
       plots,ch_prop[2,5],ch_prop[2,6], psym=7, color=cgcolor('gold'), symsize=1.5, thick=2.5
       
       xyouts, -1070,-1070,strmid(ixixdate,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5
       dcord=!D & xcord=!X & ycord=!Y
        
        if ch_prop[5,7] lt -80 then begin & ch_prop[5,7]=!values.f_nan  & ch_prop[6,7]=!values.f_nan & endif
        if ch_prop[5,8] gt 80 then begin & ch_prop[5,8]=!values.f_nan  & ch_prop[6,8]=!values.f_nan & endif
        if ch_prop[5,9] lt -80 then begin & ch_prop[5,9]=!values.f_nan  & ch_prop[6,9]=!values.f_nan & endif
        if ch_prop[5,10] gt 80 then begin & ch_prop[5,10]=!values.f_nan  & ch_prop[6,10]=!values.f_nan  & endif

      WIDGET_CONTROL, ids.vlabel_area, SET_VALUE=strtrim(STRING(ch_prop[5,0]/1e10, format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_medint, SET_VALUE=strtrim(STRING(ch_prop[5,1], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_meanint, SET_VALUE=strtrim(STRING(ch_prop[5,2], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_lng, SET_VALUE=strtrim(STRING(ch_prop[5,3], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_lat, SET_VALUE=strtrim(STRING(ch_prop[5,4], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_exlng, SET_VALUE=strtrim(STRING(ch_prop[5,7], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[5,8], format='(F15.2)'),2)
      WIDGET_CONTROL, ids.vlabel_exlat, SET_VALUE=strtrim(STRING(ch_prop[5,9], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[5,10], format='(F15.2)'),2)

      area_ratio=100.*ch_prop[6,0]/ch_prop[5.0]
      WIDGET_CONTROL, ids.vlabel_area_err, SET_VALUE=strtrim(STRING(ch_prop[6,0]/1e10, format='(F15.3)'),2)+' | '+strtrim(STRING(area_ratio, format='(F15.1)'),2)+'%'
      WIDGET_CONTROL, ids.vlabel_medint_err, SET_VALUE=strtrim(STRING(ch_prop[6,1], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_meanint_err, SET_VALUE=strtrim(STRING(ch_prop[6,2], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_lng_err, SET_VALUE=strtrim(STRING(ch_prop[6,3], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_lat_err, SET_VALUE=strtrim(STRING(ch_prop[6,4], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_exlng_err, SET_VALUE=strtrim(STRING(ch_prop[6,7], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[6,8], format='(F15.2)'),2)
      WIDGET_CONTROL, ids.vlabel_exlat_err, SET_VALUE=strtrim(STRING(ch_prop[6,9], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[6,10], format='(F15.2)'),2)
      
      if area_ratio lt 5 then begin
        trcol=[0, 127, 0]
      endif else begin
        if area_ratio gt 10 then begin
          trcol=[255,0,0]
        endif else begin
          trcol=[255, 165, 0]
        endelse
      endelse
      graycol=194
      
      WSET, ids.logoID_traffic_lights
      lights=make_array(3,16,16) 
      dist_circle, circ_struct, 16
      ;circ_struct=round(circ_struct) & 
      c_struct1=circ_struct &  c_struct1[*]=trcol[0] & c_struct1[where(circ_struct gt 8)]=graycol
      c_struct2=circ_struct &  c_struct2[*]=trcol[1] & c_struct2[where(circ_struct gt 8)]=graycol
      c_struct3=circ_struct &  c_struct3[*]=trcol[2] & c_struct3[where(circ_struct gt 8)]=graycol
      lights[0,*,*]=c_struct1 & lights[1,*,*]=c_struct2 & lights[2,*,*]=c_struct3
      tvimage, lights
      
      extracted=0
      boundary_exists=1
      widget_control, ids.save,sensitive=1
      
      endif else begin
           res=dialog_message('No Coronal Hole selected! Please reselect!', dialog_parent=ids.ex_main)
           return
      endelse

    ENDIF
    endif
    
    Dx = (double(ev.x) / dcord.X_VSIZE - xcord.s[0]) / xcord.s[1]
    DY = (double(ev.Y) / dcord.y_VSIZE - Ycord.s[0]) / ycord.s[1]
     crd=[Dx,Dy]
    curr_point=[crd[0]/windex.cdelt1+windex.crpix1,crd[1]/windex.cdelt2+windex.crpix2]
    WIDGET_CONTROL, ids.label1, SET_VALUE='X position: ' + STRING(crd[0])
    WIDGET_CONTROL, ids.label2, SET_VALUE='Y position: ' + STRING(crd[1])
    if  crd[0] le -1099 or crd[0] ge 1099 or crd[1] le -1099 or crd[1] ge 1099 then begin
      WIDGET_CONTROL, ids.label3, SET_VALUE='Current Intensity [DN]: '
    endif else begin
      WIDGET_CONTROL, ids.label3, SET_VALUE='Current Intensity [DN]: ' + STRING(workmap.data[curr_point[0],curr_point[1]])
    endelse
  ENDIF

  CASE uvalue OF
    ;##### File Management
    'files':
    'ld_text':
    'sv_text':
    'cbox_reso':
    'lbc':begin &  widget_control, ev.id, get_value=value   & if value eq 'LBC off' then widget_control, ev.id, set_value='LBC on' else widget_control, ev.id, set_value='LBC off' &  end
    'refresh': begin
      widget_control,/hourglass
      widget_control, ids.sv_path, get_value=temp_path
      lfiles=file_search([temp_path+'*.fits',temp_path+'*.fts',temp_path+'*.sav',temp_path+'efz*'])
      lfiles=file_basename(lfiles)
      widget_control, ids.files_list, set_value=lfiles
    end
    'ld_path': begin
      widget_control, ids.ld_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, ids.ld_path, set_value=temp_path
      lfiles=file_search([temp_path+'*.fits',temp_path+'*.fts',temp_path+'*.sav',temp_path+'efz*'])
      lfiles=file_basename(lfiles)
      widget_control, ids.files_list, set_value=lfiles
    end
    'sv_path': begin
      widget_control, ids.sv_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, ids.sv_path, set_value=temp_path
    end
    'load':begin
      widget_control, /hourglass
     val=WIDGET_info(ids.files_list, /list_select)
     if val eq -1 then begin & res=dialog_message( 'No file selected!', dialog_parent=ids.ex_main) & return & endif
     widget_control, ids.ld_path, get_value=temp_path
     fil=temp_path+lfiles[val]
     ;print, fil
     ;#Savefile
     if strmatch(fil, '*.sav',/fold_case) eq 1 then goto, savefile
     
     ;#SDO
     if strmatch(fil, '*.fits',/fold_case) eq 1 then begin
      
       fits_info, fil, /silent, n_ext=ext
       if ext eq 0 then index=headfits(fil,errmsg=errmag,/silent) else index=headfits(fil, /ex,errmsg=errmag,/silent)
      
      if strmatch(sxpar(index, 'origin'),'*SDO*') ne 1 then  begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
      if sxpar(index, 'WAVELNTH') ne 193 then  begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
      read_sdo, fil, header_help, data_help,/uncomp_del,/quiet
      aia_prep, header_help, data_help, header,data, /norm,/quiet
      index2map, header, float(data), themap
      map_identifyer='SDO'
     endif
     
     ;#STEREO
     if strmatch(fil, '*.fts',/fold_case) eq 1 then begin
       
       fits_info, fil, /silent, n_ext=ext
       if ext eq 0 then index=headfits(fil,errmsg=errmag,/silent) else index=headfits(fil, /ex,errmsg=errmag,/silent)
       if strmatch(sxpar(index, 'detector'),'*EUVI*') ne 1 then  begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
       if sxpar(index, 'WAVELNTH') ne 195 then begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
       secchi_prep, fil, header, data,/rotate_on,/quiet 
       index2map, header, float(data), themap
       map_identifyer='STEREO'
     endif
     
     ;#SOHO
     if strmatch(fil, '*efz*',/fold_case) eq 1 then begin
       
       fits_info, fil, /silent, n_ext=ext
       if ext eq 0 then index=headfits(fil,errmsg=errmag,/silent) else index=headfits(fil, /ex,errmsg=errmag,/silent)
       
       if strmatch(sxpar(index, 'instrume'),'EIT') ne 1 then  begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
       if sxpar(index, 'WAVELNTH') ne 195 then  begin & res=dialog_message( 'No usable file selected!', dialog_parent=ids.ex_main) & return & endif
     read_eit, fil, index,data
     eit_prep, index, data=data, nindex, ndata,/normalize, /filter_norm, /response_norm
     index2map, nindex, float(ndata), themap
     map_identifyer='SOHO'
     endif
     

      
      reso=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      if map_identifyer  eq 'SOHO'then begin 
          if reso eq '4096x4096' or reso eq '2048x2048' then begin
            res=dialog_message( ['                                   ',$
                                 'Resolution is greater than in file!',$
                                 '     Do you want to continue!      ',$
                                 '                                   '], dialog_parent=ids.ex_main,/question)
            if res eq 'No' then return
          endif
      endif
      
      if map_identifyer  eq 'STEREO'then begin
        if reso eq '4096x4096' then begin
          res=dialog_message( ['                                   ',$
                               'Resolution is greater than in file!',$
                               '     Do you want to continue!      ',$
                               '                                   '], dialog_parent=ids.ex_main,/question)
          if res eq 'No' then return
        endif
      endif
      
      case reso of
        '4096x4096': 
        '2048x2048': themap=rebin_map(themap, 2048,2048)
        '1024x1024': themap=rebin_map(themap, 1024,1024)
        '512x512'  : themap=rebin_map(themap, 512,512)
        '256x256'  : themap=rebin_map(themap, 256,256)
      endcase
      
      widget_control, ids.lbc, get_value=lbc_val
      if  lbc_val eq 'LBC on' then begin workmap=themap & workmap=annulus_limb_correction(workmap) & endif else workmap=themap
      
      workmap=rem_map_limb(workmap, missing=!values.f_nan)
      map2index, workmap, windex
      
      if 1 eq 2 then begin & savefile: 
      ;idlsav_file=obj_new('IDL_Savefile',fil)
      ;res = idlsav_file->Names() 
      restore, fil, description=des
      reso=strtrim(string(n_elements(themap.data[*,0])),2)+'x'+strtrim(string(n_elements(themap.data[*,0])),2)
      ;print, reso
      if des ne 'CATCH:EUV' then begin &  res=dialog_message( 'No CATCH save file selected!', dialog_parent=ids.ex_main) & return & endif
      ;if res[0] eq 'themap' and res[1] eq 'workmap' then begin & restore, fil & endif else begin &  res=dialog_message( 'No CATCH save file selected!', /center) & return & endelse
     endif 
      
      widget_control, ids.activate,sensitive=1
      widget_control, ids.morph,sensitive=1
      widget_control, ids.thresh,sensitive=1
      widget_control, ids.insitu,sensitive=0
      
      pmap=themap
      plot_scl=make_array(4)
      plot_scl[3]=paths.gridsize
      plot_scl[2]=paths.cthick
      
      WSET, ids.drawID
      case map_identifyer of
        'SOHO': begin
          eit_colors, 195,/silent
          dmin=paths.eit_range[0]
          dmax=paths.eit_range[1]
          logval=1    
          end
        'STEREO': begin
          SECCHI_COLORS, 'EUVI', 195, /LOAD
          dmin=paths.stereo_range[0]
          dmax=paths.stereo_range[1]
          logval=1          
          end
        'SDO': begin
          aia_lct, wave='193', /load
          dmin=paths.aia_range[0]
          dmax=paths.aia_range[1]
          logval=1
          end 
      endcase
        plot_scl[0]=dmin & plot_scl[1]=dmax
      
      ixixdate=anytim(themap.time,/ccsds)
      plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1],log=logval, grid_spacing=plot_scl[3] 
      xyouts, -1070,-1070,strmid(ixixdate,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5
      dcord=!D & xcord=!X & ycord=!Y
      
      WSET, ids.draw_histID
        sd_ind=median(workmap.data)
        h=histogram(workmap.data, locations=bin, binsize=5)
        if min(h[where(bin lt 250)],/nan)*0.8 gt 1 then ymin=min(h[where(bin lt 250)],/nan)*0.8 else ymin=1
      plot, bin, h, xtit='Intensity', /ylog, title=' ', position=[0.01,0.16,0.87,0.95],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=9,charsize=0.9, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, xrange=[0,250], yrange=[ymin,max(h,/nan)*1.2],$
        YTICKFORMAT="(A1)", ytit=' '
      xyouts, 0.995, 0.034,'I [DN]',color=cgcolor('black'),charsize=0.9,charthick=1.5,/norm,alignment=1
     
       widget_control, ids.thresh, get_value=thr_value
       oplot, [thr_value,thr_value]*sd_ind/100.,[ymin,max(h,/nan)*1.2], color=cgcolor('red'), thick=2
      polyfill,[0.688,0.688,1,1],[0.85,1,1,0.85],/norm, /fill,color=cgcolor('white')
      xyouts, 0.995, 0.88,'Threshold = '+strtrim(string(thr_value*sd_ind/100,format='(I)'),2)+' [DN]',color=cgcolor('black'),charsize=1.1,charthick=1.5,/norm,alignment=1


       WSET, ids.logoID_traffic_lights
       graycol=194
       lights=make_array(3,16,16) & lights[0,*,*]=graycol & lights[1,*,*]=graycol & lights[2,*,*]=graycol
       tvimage, lights
       
       WIDGET_CONTROL, ids.vlabel_area, SET_VALUE= ' '
       WIDGET_CONTROL, ids.vlabel_medint, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_meanint, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_lng, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_lat, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_exlng, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_exlat, SET_VALUE=' '

       WIDGET_CONTROL, ids.vlabel_area_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_medint_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_meanint_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_lng_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_lat_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_exlng_err, SET_VALUE=' '
       WIDGET_CONTROL, ids.vlabel_exlat_err, SET_VALUE=' '
       
      widget_control, ids.popt, sensitive=1
      widget_control, ids.draww,sensitive=1
      widget_control, ids.draw_tool, sensitive=1
      
       WSET, ids.drawID
       date_map=ixixdate
       boundary_exists=0
      end
    ;#### Extraction Parameters
    'appl': begin
      
        widget_control,/hourglass
        widget_control, ids.thresh, get_value=thr_value
        widget_control, ids.morph, get_value=morph_value
      
      sd_ind=median(workmap.data)
      bmap=workmap & bmap.data[*]=0 & bmaps=replicate(bmap, 5) ;& bmap2=workmap & bmap2.data[*]=0 & bmap3=workmap & bmap3.data[*]=0
      bmaps[0].data[where(workmap.data lt ((thr_value-2)/1e2*sd_ind))]=1
      bmaps[1].data[where(workmap.data lt ((thr_value-1)/1e2*sd_ind))]=1
      bmaps[2].data[where(workmap.data lt ((thr_value)/1e2*sd_ind))]=1
      bmaps[3].data[where(workmap.data lt ((thr_value+1)/1e2*sd_ind))]=1
      bmaps[4].data[where(workmap.data lt ((thr_value+2)/1e2*sd_ind))]=1
      
      if morph_value ne 0 then  begin
      dist_circle, circ_struct, 30
      c_struct=circ_struct &  c_struct[*]=1
      c_struct[where(circ_struct gt morph_value)]=0
      
        for i=0, 4 do begin  
          bmaps[i].data=morph_open(bmaps[i].data,c_struct)
          bmaps[i].data=morph_close(bmaps[i].data,c_struct)
        endfor
      endif
      
      WSET, ids.drawID

      pmap=themap
      case map_identifyer of
        'SOHO': eit_colors, 195,/silent
        'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
        'SDO': aia_lct, wave='193', /load
      endcase


      ixixdate=anytim(themap.time,/ccsds)
      plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1], /log,title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3] 
      plot_map,  bmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[2]
      xyouts, -1070,-1070,strmid(ixixdate,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5 
      dcord=!D & xcord=!X & ycord=!Y
      
      WIDGET_CONTROL, ids.vlabel_area, SET_VALUE= ' '
      WIDGET_CONTROL, ids.vlabel_medint, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_meanint, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_lng, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_lat, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_exlng, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_exlat, SET_VALUE=' '

      WIDGET_CONTROL, ids.vlabel_area_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_medint_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_meanint_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_lng_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_lat_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_exlng_err, SET_VALUE=' '
      WIDGET_CONTROL, ids.vlabel_exlat_err, SET_VALUE=' '
      
      WSET, ids.logoID_traffic_lights

      graycol=194
      lights=make_array(3,16,16) & lights[0,*,*]=graycol & lights[1,*,*]=graycol & lights[2,*,*]=graycol
      tvimage, lights
      
      widget_control, ids.save,sensitive=0
      extracted =1
      
     end
    'thresh':begin
      WSET, ids.draw_histID
      sd_ind=median(workmap.data)
      h=histogram(workmap.data, locations=bin, binsize=5)
      if min(h[where(bin lt 250)],/nan)*0.8 gt 1 then ymin=min(h[where(bin lt 250)],/nan)*0.8 else ymin=1
      plot, bin, h, xtit='Intensity', /ylog, title=' ', position=[0.01,0.16,0.87,0.95],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=9,charsize=0.9, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, xrange=[0,250], yrange=[ymin,max(h,/nan)*1.2],$
        YTICKFORMAT="(A1)", ytit=' '
      xyouts, 0.995, 0.034,'I [DN]',color=cgcolor('black'),charsize=0.9,charthick=1.5,/norm,alignment=1

      widget_control, ids.thresh, get_value=thr_value
      oplot, [thr_value,thr_value]*sd_ind/100.,[ymin,max(h,/nan)*1.2], color=cgcolor('red'), thick=2
      polyfill,[0.688,0.688,1,1],[0.85,1,1,0.85],/norm, /fill,color=cgcolor('white')
      xyouts, 0.995, 0.88,'Threshold = '+strtrim(string(thr_value*sd_ind/100,format='(I)'),2)+' [DN]',color=cgcolor('black'),charsize=1.1,charthick=1.5,/norm,alignment=1

       
       WSET, ids.drawID
      
      
     end
    'morph' :
    'draw':
    'insitu': begin
      widget_control,/hourglass
      widget_control, ids.ex_main, TLB_GET_OFFSET=offs

      insi_main = widget_base(title='CATCH: In-Situ Data', xsize=560, ysize=735,SCR_XSIZE=560,SCR_YSIZE=735,modal=(1-debug), group_leader=ids.ex_main, xoffset=offs[0]+220, yoffset=offs[1]-20)

      top_insi = Widget_Base(insi_main, XOFFSET=10)
      label = Widget_Label(top_insi, value='In-Situ Data', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      insi = Widget_Base(top_insi, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=715,xsize=540)
      label = Widget_Label(top_insi, value='In-Situ Data', XOFFSET=10)

      min_date=anytim(anytim(date_map)-24.*3600.*5.,/ccsds) & max_date=anytim(anytim(date_map)+24.*3600.*20.,/ccsds)
      
      if map_identifyer eq 'STEREO' then begin
        if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then begin & obs= map_identifyer+'_A' & sat='STA' & endif
        if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then begin & obs= map_identifyer+'_B' & sat='STB' & endif
        
      ;res=dialog_message('Stereo In-Situ data not yet available!', dialog_parent=ids.ex_main)
      ;return
        
        
        
        file_unzip, dir+'stereo_insitu_data.zip',files=flist
        
        yr=strmid(min_date, 0,4)
        match1=strmatch(flist,'*'+sat+'*PLASTIC*'+yr+'*', /fold_case)
        match2=strmatch(flist,'*'+sat+'*MAG*'+yr+'*', /fold_case)
        
        l_index1=where(match1 eq 1) 
        if l_index1 ge 0 then begin
        plasma_file=flist[l_index1]
        endif else begin
        res=dialog_message('No STEREO data available!', dialog_parent=ids.ex_main)
            return
          endelse
          
        l_index2=where(match2 eq 1) & mag_file=flist[l_index2]
        if l_index2 ge 0 then begin
          mag_file=flist[l_index2]
           endif else begin
          res=dialog_message('No STEREO data available!', dialog_parent=ids.ex_main)
          return
        endelse
        

openr, lun, plasma_file, /get_lun
line = list()  ;search for end of header (= 'DATA')
line_tmp = ''
n = 0
while line_tmp ne 'DATA:' do begin
  readf, lun, line_tmp
  line.add, line_tmp
  n++
endwhile

close,/all
readcol, plasma_file, yr,mon,dy,hr,mn,sec,dummy,density, speed, temp,dummy,dummy,dummy,format='(I4.2,I4.3,I3.2,I3.2,I3.2,I3.2,I4.3,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5)', /silent,SKIPLINE=n
density[where(density eq 1e34)]=!values.f_nan & speed[where(speed eq 1e34)]=!values.f_nan  &  temp[where(temp eq 1e34)]=!values.f_nan
atim=anytim(string(yr,f='(I4.4)')+'-'+string(mon,f='(I2.2)')+'-'+string(dy,f='(I2.2)')+'T'+string(hr, f='(I2.2)')+':'+string(mn,f='(I2.2)')+':'+string(sec,f='(I2.2)'))
date_plasma=anytim(atim,/ccsds)


openr, lun, mag_file, /get_lun
line = list()  ;search for end of header (= 'DATA')
line_tmp = ''
n = 0
while line_tmp ne 'DATA:' do begin
  readf, lun, line_tmp
  line.add, line_tmp
  n++
endwhile

close,/all
readcol, mag_file, yr,mon,dy,hr,mn,sec,dummy,br,bt,bn,btsc,dummy,dummy,dummy,dummy,format='(I4.2,I4.3,I3.2,I3.2,I3.2,I3.2,I4.3,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5)', /silent,SKIPLINE=n
br[where(br eq 1e34)]=!values.f_nan & bt[where(bt eq 1e34)]=!values.f_nan & bn[where(bn eq 1e34)]=!values.f_nan & btsc[where(btsc eq 1e34)]=!values.f_nan &
atim=anytim(string(yr,f='(I4.4)')+'-'+string(mon,f='(I2.2)')+'-'+string(dy,f='(I2.2)')+'T'+string(hr, f='(I2.2)')+':'+string(mn,f='(I2.2)')+':'+string(sec,f='(I2.2)'))
date_mag=anytim(atim,/ccsds)

bx=br & by=bt & bz=bn

         

        if strmid(min_date,0,4) ne strmid(max_date,0,4) then begin
          
          backup_d=density
          backup_s=speed
          backup_t=temp
          backup_btsc=btsc
          backup_bx=bx
          backup_by=by
          backup_bz=bz
          backup_dm=date_mag
          backup_dp=date_plasma
         
       
          yr=strmid(max_date, 0,4)
          match1=strmatch(flist,'*'+sat+'*PLASTIC*'+yr+'*', /fold_case)
          match2=strmatch(flist,'*'+sat+'*MAG*'+yr+'*', /fold_case)

          l_index1=where(match1 eq 1)
          if l_index1 ge 0 then begin
            plasma_file=flist[l_index1]
          endif else begin
            res=dialog_message('No STEREO data available!', dialog_parent=ids.ex_main)
            return
          endelse

          l_index2=where(match2 eq 1) & mag_file=flist[l_index2]
          if l_index2 ge 0 then begin
            mag_file=flist[l_index2]
          endif else begin
            res=dialog_message('No STEREO data available!', dialog_parent=ids.ex_main)
            return
          endelse


          openr, lun, plasma_file, /get_lun
          line = list()  ;search for end of header (= 'DATA')
          line_tmp = ''
          n = 0
          while line_tmp ne 'DATA:' do begin
            readf, lun, line_tmp
            line.add, line_tmp
            n++
          endwhile

          close,/all
          readcol, plasma_file, yr,mon,dy,hr,mn,sec,dummy,density, speed, temp,dummy,dummy,dummy,format='(I4.2,I4.3,I3.2,I3.2,I3.2,I3.2,I4.3,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5)', /silent,SKIPLINE=n
          density[where(density eq 1e34)]=!values.f_nan & speed[where(speed eq 1e34)]=!values.f_nan  &  temp[where(temp eq 1e34)]=!values.f_nan
          atim=anytim(string(yr,f='(I4.4)')+'-'+string(mon,f='(I2.2)')+'-'+string(dy,f='(I2.2)')+'T'+string(hr, f='(I2.2)')+':'+string(mn,f='(I2.2)')+':'+string(sec,f='(I2.2)'))
          date_plasma=anytim(atim,/ccsds)
          

          openr, lun, mag_file, /get_lun
          line = list()  ;search for end of header (= 'DATA')
          line_tmp = ''
          n = 0
          while line_tmp ne 'DATA:' do begin
            readf, lun, line_tmp
            line.add, line_tmp
            n++
          endwhile

          close,/all
          readcol, mag_file, yr,mon,dy,hr,mn,sec,dummy,br,bt,bn,btsc,dummy,dummy,dummy,dummy,format='(I4.2,I4.3,I3.2,I3.2,I3.2,I3.2,I4.3,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5,F13.5)', /silent,SKIPLINE=n
          br[where(br eq 1e34)]=!values.f_nan & bt[where(bt eq 1e34)]=!values.f_nan & bn[where(bn eq 1e34)]=!values.f_nan & btsc[where(btsc eq 1e34)]=!values.f_nan &
          atim=anytim(string(yr,f='(I4.4)')+'-'+string(mon,f='(I2.2)')+'-'+string(dy,f='(I2.2)')+'T'+string(hr, f='(I2.2)')+':'+string(mn,f='(I2.2)')+':'+string(sec,f='(I2.2)'))
          date_mag=anytim(atim,/ccsds)

          bx=br & by=bt & bz=bn
        
          date_plasma=[backup_dp, date_plasma]
          date_mag=[backup_dm, date_mag]
          speed=[backup_s, speed]
          density=[backup_d, density]
          temp=[backup_t, temp]
          btsc=[backup_btsc, btsc]
          bx=[backup_bx, bx]
          by=[backup_by, by]
          bz=[backup_bz, bz]
        endif
        
        file_delete, flist,/quiet
        
        plasmadex=where(anytim(date_plasma) ge anytim(min_date) and anytim(date_plasma) le anytim(max_date))
        magdex=where(anytim(date_mag) ge anytim(min_date) and anytim(date_mag) le anytim(max_date))
        
        date_plasma=date_plasma[plasmadex]
        date_mag=date_mag[magdex]
        speed=speed[plasmadex]
        density=density[plasmadex]
        temp=temp[plasmadex]
        btsc=btsc[magdex]
        bx=bx[magdex]
        by=by[magdex]
        bz=bz[magdex]

      endif else begin
      swepamdata=get_acedata(min_date,max_date,/swepam,/monthly)
     
      speed=swepamdata.b_speed & fill_missing,speed,-9999.90,1
      density=swepamdata.p_density & fill_missing,density,-9999.90,1
      temp=swepamdata.ion_temp & fill_missing,temp,-100000,1
      magdata=get_acedata(min_date,max_date,/mag,/monthly)
      
      btsc=magdata.bt
      bx=magdata.bx
      by=magdata.by
      bz=magdata.bz

      date_plasma=anytim(mjd2any(swepamdata.mjd)+swepamdata.time/1000.,/ccsds)
      date_mag=anytim(mjd2any(magdata.mjd)+magdata.time/1000.,/ccsds)
      endelse
      
      stim=strmid(date_map,0,19)
      etim=strmid(anytim(anytim(date_map)+24.*3600.*10.,/ccsds),0,19)
      
      ranges=[min(speed,/nan)*0.9, ceil(max(speed,/nan)*1.1), min(density,/nan)*0.9, ceil(max(density,/nan)*1.1), $
            min(temp/1e5,/nan)*0.9, ceil(max(temp/1e5,/nan)*1.1), min(btsc,/nan)*0.9, ceil(max(btsc,/nan)*1.1), $
            min([bx,by,bz],/nan)*1.1, ceil(max([bx,by,bz],/nan)*1.1)]
      ranges=(ranges)
      
      start_time=widget_slider(insi,/SUPPRESS_VALUE, uval='s_time', min=0, max=480, value=120., xsize=255, xoffset=10, yoffset=10,/align_center )
      end_time=widget_slider(insi,/SUPPRESS_VALUE, uval='e_time', min=0, max=480, value=240., xsize=255, xoffset=275, yoffset=10,/align_center )

      start_time_label1 = WIDGET_LABEL(insi, XSIZE=90, VALUE='Start Time:', xoffset=10, yoffset=30,/align_left)
      start_time_label2 = WIDGET_LABEL(insi, XSIZE=160, VALUE=stim, xoffset=100, yoffset=30)
      end_time_label1 = WIDGET_LABEL(insi, XSIZE=90, VALUE='End Time:', xoffset=275, yoffset=30,/align_left)
      end_time_label2 = WIDGET_LABEL(insi, XSIZE=160, VALUE=etim, xoffset=365, yoffset=30)
      
      help_x=50.
      help_x2=help_x-10.
      
      lab1 = WIDGET_LABEL(insi, XSIZE=75, VALUE='v [km/s]', xoffset=0, yoffset=help_x2+65,/align_center)
      lab2 = WIDGET_LABEL(insi, XSIZE=75, VALUE='n [1/cm^3]', xoffset=0, yoffset=help_x2+170,/align_center)
      lab3 = WIDGET_LABEL(insi, XSIZE=75, VALUE='T [10^5 K]', xoffset=0, yoffset=help_x2+275,/align_center)
      lab4 = WIDGET_LABEL(insi, XSIZE=75, VALUE='B [nT]', xoffset=0, yoffset=help_x2+380,/align_center)
      lab5 = WIDGET_LABEL(insi, XSIZE=75, VALUE='Bi [nT]', xoffset=0, yoffset=help_x2+485,/align_center)
     
      draw_insitu1 = widget_draw(insi, uvalue='draw_1', xsize=455, ysize=100, xoffset=75, yoffset=help_x+10,/frame)
      draw_insitu2 = widget_draw(insi, uvalue='draw_2', xsize=455, ysize=100, xoffset=75, yoffset=help_x+115,/frame)
      draw_insitu3 = widget_draw(insi, uvalue='draw_3', xsize=455, ysize=100, xoffset=75, yoffset=help_x+220,/frame)
      draw_insitu4 = widget_draw(insi, uvalue='draw_4', xsize=455, ysize=100, xoffset=75, yoffset=help_x+325,/frame)
      draw_insitu5 = widget_draw(insi, uvalue='draw_5', xsize=455, ysize=100, xoffset=75, yoffset=help_x+430,/frame)
      draw_insitu6 = widget_draw(insi, uvalue='draw_5', xsize=455, ysize=50, xoffset=75, yoffset=help_x+535,/frame)
      mark_slider=widget_slider(insi,/SUPPRESS_VALUE, uval='mark', min=0, max=1000, value=400., xsize=465, xoffset=70, yoffset=help_x+600,/align_center )
      
      ;abort_insi= widget_button(insi,value='Done', uval='abort_insi',/NO_release, xsize=150. , xoffset=400., yoffset=help_x+610, tooltip='Close in-situ window',ysize=25)
      
      lab_curr = WIDGET_LABEL(insi, XSIZE=130, VALUE='Time at guideline:', xoffset=10, yoffset=help_x+635,/align_left)
      
      lab_current = WIDGET_LABEL(insi, XSIZE=180, VALUE=' ', xoffset=130, yoffset=help_x+635,/align_center)
      
      save_insi= widget_button(insi,value='Save Image', uval='save_insi',/NO_release, xsize=100. , xoffset=320., yoffset=help_x+630, tooltip='Save image to output directory',ysize=25)
      abort_insi= widget_button(insi,value='Done', uval='abort_insi',/NO_release, xsize=100. , xoffset=430., yoffset=help_x+630, tooltip='Close in-situ window',ysize=25)

      widget_control, draw_insitu1,sensitive=0
      widget_control, draw_insitu2,sensitive=0 
      widget_control, draw_insitu3,sensitive=0 
      widget_control, draw_insitu4,sensitive=0 
      widget_control, draw_insitu5,sensitive=0 
      widget_control, draw_insitu6,sensitive=0
      ;widget_control, save_insi, sensitive=0  


      widget_control, insi_main, /realize
      
      
      WIDGET_CONTROL, draw_insitu1, GET_VALUE=draw_insitu1ID
      WIDGET_CONTROL, draw_insitu2, GET_VALUE=draw_insitu2ID
      WIDGET_CONTROL, draw_insitu3, GET_VALUE=draw_insitu3ID
      WIDGET_CONTROL, draw_insitu4, GET_VALUE=draw_insitu4ID
      WIDGET_CONTROL, draw_insitu5, GET_VALUE=draw_insitu5ID
      WIDGET_CONTROL, draw_insitu6, GET_VALUE=draw_insitu6ID
      
            insi_ids={draw_insitu1ID:draw_insitu1ID,draw_insitu2ID:draw_insitu2ID,draw_insitu3ID:draw_insitu3ID,$
                draw_insitu4ID:draw_insitu4ID,draw_insitu5ID:draw_insitu5ID,draw_insitu6ID:draw_insitu6ID, lab_current:lab_current,$
                start_time:start_time_label2,end_time:end_time_label2, abort_insi:abort_insi, save_insi:save_insi,$
                mark_slider:mark_slider, start_time_slider:start_time, end_time_slider:end_time,insi_main:insi_main }
                
      widget_control, insi_ids.mark_slider, get_value=slider_val  &   tr=(anytim(etim)-anytim(stim)) & lineval=anytim(anytim(stim)+(tr*float(slider_val))/1e3,/ccsds)
      widget_control, insi_ids.start_time, get_value=stim
      widget_control, insi_ids.end_time, get_value=etim
      widget_control, insi_ids.lab_current, set_value=strmid(lineval,0,19)
      
WSET, insi_ids.draw_insitu1ID
      !p.color=cgcolor('black')
      utplot, date_plasma, speed, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[0],ranges[1]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[0],ranges[1]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[0],ranges[1]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu2ID
      !p.color=cgcolor('black')
      utplot, date_plasma, density, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[2],ranges[3]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[2],ranges[3]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[2],ranges[3]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu3ID
      !p.color=cgcolor('black')
      utplot, date_plasma, temp/1e5, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[4],ranges[5]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[4],ranges[5]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[4],ranges[5]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu4ID
      !p.color=cgcolor('black')
      utplot, date_mag, btsc, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[6],ranges[7]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[6],ranges[7]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[6],ranges[7]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu5ID
      !p.color=cgcolor('black')
      utplot, date_mag, bx, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[8],ranges[9]], timerange=[stim,etim],/nodata
      outplot, [stim,etim], [0,0], color=cgcolor('dark gray'), thick=1.5, linestyle=2
      outplot, date_mag, bx, color=cgcolor('red'), thick=1.5
      outplot, date_mag, by, color=cgcolor('green'), thick=1.5
      outplot, date_mag, bz, color=cgcolor('blue'), thick=1.5
      outplot, [lineval,lineval],[ranges[8],ranges[9]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[8],ranges[9]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      if map_identifyer eq 'STEREO' then begin
        xyouts, 0.85,0.87,'B!Dr!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
        xyouts, 0.9,0.87,'B!Dt!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
        xyouts, 0.95,0.87,'B!Dn!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endif else begin
      xyouts, 0.85,0.87,'B!Dx!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
      xyouts, 0.9,0.87,'B!Dy!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
      xyouts, 0.95,0.87,'B!Dz!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endelse
      
      WSET, insi_ids.draw_insitu6ID
      !p.color=cgcolor('black')
      rndvec=randomu(seed, n_elements(date_mag))
      utplot, date_mag, rndvec, xtit='Time (Starting '+stim+' )', ytit=' ', title=' ', position=[0.08,0.7,0.99,0.99],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=4,charsize=1, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[0,1], timerange=[stim,etim], /nodata,xticklen=!D.y_VSIZE/100., xminor=4
      outplot, [lineval,lineval],[-2,1], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[-2,1], color=cgcolor('blue'), thick=1, linestyle=2
      
      
      xmanager, 'insi',insi_main,  /no_block
    end
    
    'poptions': begin
      widget_control, ids.ex_main, TLB_GET_OFFSET=offs

      opt_main = widget_base(title='Options', xsize=170, ysize=220,SCR_XSIZE=170,SCR_YSIZE=220,modal=(1-debug), group_leader=ids.ex_main, xoffset=offs[0]+634, yoffset=offs[1]+171, uvalue='poptex')
      
      top_opt = Widget_Base(opt_main, XOFFSET=10)
      label = Widget_Label(top_opt, value='Plot Options', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      opt = Widget_Base(top_opt, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=200,xsize=150)
      label = Widget_Label(top_opt, value='Plot Options', XOFFSET=10)
      

      label_dmin = WIDGET_LABEL(opt, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text=widget_TEXT(opt,value=strtrim(string(plot_scl[0],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=10, ysize=1, uvaL='dmin_text', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(opt, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text=widget_TEXT(opt,value=strtrim(string(plot_scl[1],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=50, ysize=1, uvaL='dmax_text', units=0,/editable,/ALL_EVENTS)
      label_grid = WIDGET_LABEL(opt, XSIZE=50, VALUE='Grid:', xoffset=10, yoffset=95, /align_left)
      grid_text=widget_TEXT(opt,value=strtrim(string(plot_scl[3],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=90, ysize=1, uvaL='grid_text', units=0,/editable,/ALL_EVENTS)
      label_thick = WIDGET_LABEL(opt, XSIZE=50, VALUE='Thick:', xoffset=10, yoffset=135, /align_left)
      thick_text=widget_TEXT(opt,value=strtrim(string(plot_scl[2],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=135, ysize=1, uvaL='thick_text', units=0,/editable,/ALL_EVENTS)
      
      abort_opt= widget_button(opt,value='Abort', uval='abort_opt',/NO_release, xsize=60 , xoffset=10., yoffset=170, tooltip='Close options window',ysize=25)
      apply_opt= widget_button(opt,value='Apply', uval='ap_opt',/NO_release, xsize=60 , xoffset=80., yoffset=170, tooltip='Apply options',ysize=25)
      
      ids.opt_main=opt_main & ids.dmin_text=dmin_text & ids.grid_text=grid_text
      ids.dmax_text=dmax_text & ids.apply_opt=apply_opt & ids.abort_opt=abort_opt & ids.thick_text=thick_text
      
      
      widget_control, opt_main, /realize
      
      xmanager, 'opt',opt_main,  /no_block
      end
      
     'draw_tool': begin
       widget_control, ids.ex_main, TLB_GET_OFFSET=offs

       draw_main = widget_base(title='Drawing Tool', xsize=768+150, ysize=788,SCR_XSIZE=768+150,SCR_YSIZE=788,modal=(1-debug), group_leader=ids.ex_main, xoffset=offs[0]+41, yoffset=offs[1]-44)
       
       draw_big = widget_draw(draw_main, uvalue='draw_big',/MOTION_EVENTS,xsize=512*1.5, ysize=512*1.5, xoffset=140, yoffset=10,/button,/frame)
       
       
       top_draw = Widget_Base(draw_main, XOFFSET=10)
       label = Widget_Label(top_draw, value='Brush', XOFFSET=10)
       labelGeometry = Widget_Info(label, /GEOMETRY)
       labelYSize =  labelGeometry.ysize
       drawi = Widget_Base(top_draw, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=130,xsize=120)
       label = Widget_Label(top_draw, value='Brush', XOFFSET=10)
       
       draw_erase= widget_button(drawi,value='Drawing Mode', uval='toggle_draw',/NO_release, xsize=100 , xoffset=10., yoffset=20, tooltip='Toggle between drawing mode and eraser',ysize=25)       
       draw_radius=widget_slider(drawi, title='   Brush Size', uval='brush_draw', min=1., max=100., value=5., xsize=100, xoffset=10, yoffset=70,/align_center ) 
       
       
       top_draw = Widget_Base(draw_main, XOFFSET=10, yoffset=150)
       label = Widget_Label(top_draw, value='Smooth', XOFFSET=10)
       labelGeometry = Widget_Info(label, /GEOMETRY)
       labelYSize =  labelGeometry.ysize
       drawi2 = Widget_Base(top_draw, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=130,xsize=120)
       label = Widget_Label(top_draw, value='Smooth', XOFFSET=10)

       draw_morph_activ= widget_button(drawi2,value='Smooth Edges', uval='activate_morph',/NO_release, xsize=100 , xoffset=10., yoffset=20, tooltip='Apply morphological operators to the boundary',ysize=25)              
       draw_morph=widget_slider(drawi2,  title='   Morph Size', uval='morph_draw', min=0., max=15., value=2., xsize=100, xoffset=10, yoffset=70,/align_center )
       

       top_draw = Widget_Base(draw_main, XOFFSET=10, yoffset=300)
       label = Widget_Label(top_draw, value='New', XOFFSET=10)
       labelGeometry = Widget_Info(label, /GEOMETRY)
       labelYSize =  labelGeometry.ysize
       drawi3 = Widget_Base(top_draw, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=60,xsize=120)
       label = Widget_Label(top_draw, value='New', XOFFSET=10)

       new_draw= widget_button(drawi3,value='New Mask', uval='new_draw',/NO_release, xsize=100 , xoffset=10., yoffset=20, tooltip='Delete current mask',ysize=25)


       top_draw = Widget_Base(draw_main, XOFFSET=10, yoffset=380)
       label = Widget_Label(top_draw, value='Plot', XOFFSET=10)
       labelGeometry = Widget_Info(label, /GEOMETRY)
       labelYSize =  labelGeometry.ysize
       drawi4 = Widget_Base(top_draw, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=60,xsize=120)
       label = Widget_Label(top_draw, value='Plot', XOFFSET=10)

       popt_draw= widget_button(drawi4,value='Plot Options', uval='popt_draw',/NO_release, xsize=100 , xoffset=10., yoffset=20, tooltip='Configure plot',ysize=25)
       
              
       top_draw = Widget_Base(draw_main, XOFFSET=10, yoffset=788-120)
       label = Widget_Label(top_draw, value='Finish', XOFFSET=10)
       labelGeometry = Widget_Info(label, /GEOMETRY)
       labelYSize =  labelGeometry.ysize
       drawi4 = Widget_Base(top_draw, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=100,xsize=120)
       label = Widget_Label(top_draw, value='Finish', XOFFSET=10)
       
             
       abort_draw= widget_button(drawi4,value='Abort', uval='abort_draw',/NO_release, xsize=100 , xoffset=10., yoffset=20, tooltip='Close options window',ysize=25)
       apply_draw= widget_button(drawi4,value='Apply', uval='ap_draw',/NO_release, xsize=100 , xoffset=10., yoffset=65, tooltip='Return mask for calculation',ysize=25)
       
       ;widget_control, draw_big, sensitive=0
       widget_control, draw_main, /realize
       
       WIDGET_CONTROL, draw_big, GET_VALUE=draw_bigID
        
       ids.draw_main=draw_main & ids.apply_draw=apply_draw & ids.abort_draw=abort_draw & ids.draw_big=draw_big 
       ids.draw_radius=draw_radius & ids.popt_draw=popt_draw & ids.new_draw=new_draw & ids.draw_morph=draw_morph
       ids.draw_morph_activ=draw_morph_activ &  & ids.draw_erase=draw_erase & ids.draw_bigID=draw_bigID
       

       WSET, ids.draw_bigID
       pmap=themap
       case map_identifyer of
         'SOHO': eit_colors, 195,/silent
         'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
         'SDO': aia_lct, wave='193', /load
       endcase
       
       ixixdate=anytim(themap.time,/ccsds)

       plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255
             
       if boundary_exists eq 1 then begin
       draw_binmap=chmaps[2]
       plot_map, draw_binmap, /over, /cont, color=cgcolor('red'),/overlay, contour=1,/cell_fill,levels=[1]
       endif else begin
       draw_binmap=themap 
       draw_binmap.data[*]=0 
       endelse
       map2index, draw_binmap, draw_dex

      dcord2=!D & xcord2=!X & ycord2=!Y
      mousebutton = 0 
      
       xmanager, 'drawi',draw_main,  /no_block
      end
      
    ;#### Finish
    'save': begin
      WIDGET_CONTROL, /HOURGLASS
      widget_control, ids.sv_path, get_value=temp_path
      if file_test(temp_path,/directory) ne 1 then begin
          res=dialog_message('No output directory specified!', dialog_parent=ids.ex_main)
          return
      endif
      map2index, workmap, index
      map2index, chmaps[4], chdex
      ind=array_indices(chmaps[4].data, where(chmaps[4].data eq 1))
      sub_map,  chmaps,binmap, xrange=[(min(ind[0,*])-chdex.crpix1)*chdex.cdelt1-50,(max(ind[0,*])-chdex.crpix1)*chdex.cdelt1+50], $
        yrange=[(min(ind[1,*])-chdex.crpix2)*chdex.cdelt2-50,(max(ind[1,*])-chdex.crpix2)*chdex.cdelt2+50]
        
      final_binmap=binmap[0]  
      for ji=1, 4 do final_binmap.data[where(binmap[ji].data ne binmap[(ji-1)].data)]=1+ji
    
      
      if map_identifyer eq 'SOHO' or map_identifyer eq 'SDO' then begin & obs= map_identifyer & endif else begin
        if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then obs= map_identifyer+'_A'
        if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then obs= map_identifyer+'_B'
      endelse
      
      sd_ind=median(workmap.data)
      


      map2index, final_binmap, bindex, bdata
      bindex=ADD_TAG(bindex,ch_prop[0,0],'area_1')
      bindex=ADD_TAG(bindex,ch_prop[1,0],'area_2')
      bindex=ADD_TAG(bindex,ch_prop[2,0],'area_3')
      bindex=ADD_TAG(bindex,ch_prop[3,0],'area_4')
      bindex=ADD_TAG(bindex,ch_prop[4,0],'area_5')
      bindex=ADD_TAG(bindex,thr_value,'thresh')
      bindex=ADD_TAG(bindex,thr_value*sd_ind/100.,'threshdn')
      bindex=ADD_TAG(bindex,version,'v_CATCH')
      
      helpdate=anytim(index.date_obs,/ex)
      yyyy=strtrim(string(helpdate[6]),2)
      if helpdate[5] lt 10 then mm='0'+strtrim(string(helpdate[5]),2) else mm=strtrim(string(helpdate[5]),2)
      if helpdate[4] lt 10 then dd='0'+strtrim(string(helpdate[4]),2) else dd=strtrim(string(helpdate[4]),2)
      if helpdate[0] lt 10 then hh='0'+strtrim(string(helpdate[0]),2) else hh=strtrim(string(helpdate[0]),2)
      if helpdate[1] lt 10 then mimi='0'+strtrim(string(helpdate[1]),2) else mimi=strtrim(string(helpdate[1]),2)
      if helpdate[2] lt 10 then ss='0'+strtrim(string(helpdate[2]),2) else ss=strtrim(string(helpdate[2]),2)
      date_format=yyyy+mm+dd+'_'+hh+mimi+ss
      ;print, ss
      dir_name=date_format+'_CATCH/'
      if FILE_TEST(temp_path+dir_name, /DIRECTORY) eq 0 then file_mkdir, temp_path+dir_name
      
      outname=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_CATCH_binary_ch_mask.fits' 
      outname_prop=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_ch_euv_properties.txt'
      outname_save=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmaps.sav'
      outname_fd=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_FD.eps'
      outname_euv=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap.eps'
      outname_fdpng=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_FD.png'
      outname_euvpng=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap.png'
      
      if file_test(outname) eq 1 then begin
                   res=dialog_message( ['                                    ',$
                                        '       File already exists!         ',$
                                        '     Do you want to overwrite!      ',$
                                        '                                    '], dialog_parent=ids.ex_main,/question)
       if res eq 'No' then begin
        file_number=1
        while file_test(outname) eq 1 do begin
          file_number++
          outname=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_CATCH_binary_ch_mask_'+strtrim(string(file_number),2)+'.fits'
        endwhile
       
       outname_prop=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_ch_euv_properties_'+strtrim(string(file_number),2)+'.txt'
       outname_save=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmaps_'+strtrim(string(file_number),2)+'.sav'
       outname_fd=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_FD_'+strtrim(string(file_number),2)+'.eps'
       outname_euv=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_'+strtrim(string(file_number),2)+'.eps'
       outname_fdpng=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_FD_'+strtrim(string(file_number),2)+'.png'
       outname_euvpng=temp_path+dir_name+date_format+'_'+obs+'_'+reso+'_euvmap_'+strtrim(string(file_number),2)+'.png'
       endif
      
      endif 
   
      mwritefits, bindex, bdata, outfile = outname
      chproperties2txt, ch_prop,[thr_value,thr_value*sd_ind/100.], outname_prop, obs, strmid(index.date_obs,0,19)
      
      if paths.smaps_euv eq 'y' then save, filename=outname_save, description='CATCH:EUV', themap, workmap, binmap, map_identifyer
      
      if paths.euv_eps eq 'y' then begin
        

      x_size=10       ;x size in cm
      y_size=10     ;x size in cm
      x_offset=0
      y_offset=0
      
      
      case map_identifyer of
        'SOHO': eit_colors, 195,/silent
        'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
        'SDO': aia_lct, wave='193', /load
      endcase


      SET_PLOT, 'PS'
      device, filename=outname_fd,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0,BITS_PER_PIXEL=24
      pmap=themap
      plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(index.date_obs,0,16),$
        position=[0.2,0.2,0.9,0.9],grid_spacing=plot_scl[2],charthick=2,charsize=1,/log 
      plot_map, chmaps[2], /over,/cont, color=cgcolor('red'),c_thick=plot_scl[2]
      plots,ch_prop[2,5],ch_prop[2,6], psym=7, color=cgcolor('gold'), symsize=0.5, thick=2

      DEVICE, /CLOSE_FILE
      SET_PLOT, 'X'
      
      SET_PLOT, 'PS'
      device, filename=outname_euv,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0,BITS_PER_PIXEL=24
      sub_map, themap,pmap, ref_map=binmap

      plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(index.date_obs,0,16),$
        grid_spacing=plot_scl[3],charthick=2,charsize=0.7,/log 
      plot_map, chmaps[2], /over,/cont, color=cgcolor('red'),c_thick=plot_scl[2]
      plots,ch_prop[2,5],ch_prop[2,6], psym=7, color=cgcolor('gold'), symsize=0.8, thick=2

      DEVICE, /CLOSE_FILE
      SET_PLOT, 'X'
      
      endif
      
      if paths.euv_png eq 'y' then begin
   
   Set_Plot, 'Z'
   Device, Decomposed=1, Set_Pixel_Depth=24, Set_Resolution=[2160,2160];,decomposed=1;,BITS_PER_PIXEL=24;,color=1
   !p.multi=0
   
   !P.Color = '000000'xL
   !P.Background = 'FFFFFF'xL
   
        case map_identifyer of
          'SOHO': eit_colors, 195,/silent
          'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
          'SDO': aia_lct, wave='193', /load
        endcase

        pmap=themap
        plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(index.date_obs,0,19),$
          position=[0.2,0.2,0.9,0.9],grid_spacing=plot_scl[3],charthick=6,charsize=4,/log
        plot_map, chmaps[2], /over,/cont, color=cgcolor('red'),c_thick=plot_scl[2]
        plots,ch_prop[2,5],ch_prop[2,6], psym=7, color=cgcolor('gold'), symsize=3, thick=4

        write_png, outname_fdpng,  tvrd(/true)


        sub_map, themap,pmap, ref_map=binmap

        plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(index.date_obs,0,19),$     
         grid_spacing=plot_scl[3],charthick=6,charsize=4,/log
        plot_map, chmaps[2], /over,/cont, color=cgcolor('red'),c_thick=plot_scl[2]
        plots,ch_prop[2,5],ch_prop[2,6], psym=7, color=cgcolor('gold'), symsize=3, thick=5
        
        write_png, outname_euvpng,  tvrd(/true)
        
      endif  


Set_Plot, 'X'


        res=dialog_message( ['                           ',$
                             ' Files sucessfully saved!  ',$
                             '                           ',$
                             ' Binary map       [y]      ',$
                             ' Properties       [y]      ',$
                             ' Maps save file   ['+strtrim(string(paths.smaps_euv),2)+']      ',$
                             ' .eps Images      ['+strtrim(string(paths.euv_eps),2)+']      ',$
                             ' .png Images      ['+strtrim(string(paths.euv_png),2)+']      ',$
                             '                           '], dialog_parent=ids.ex_main,/information)
        
      end
    'abort': begin 
      
     
      IF paths.lock eq 'off' then begin
      new_paths=paths
      changes=0
      widget_control, ids.ld_path, get_value=temp_path
      if temp_path ne paths.euvpath then begin & changes+=1 & new_paths.euvpath=temp_path & endif
      widget_control, ids.sv_path, get_value=temp_path
      if temp_path ne paths.outpath then begin & changes+=1 & new_paths.outpath=temp_path & endif

      widget_control, ids.lbc, get_value=lbc_val
      if lbc_val eq 'LBC off' then lbc_val='off'
      if lbc_val eq 'LBC on' then lbc_val='on'
      if lbc_val ne paths.lbc then begin & changes+=1 & new_paths.lbc=lbc_val & endif
      resol=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      case resol of
        '4096x4096': res_val=4096
        '2048x2048': res_val=2048
        '1024x1024': res_val=1024
        '512x512'  : res_val=512
        '256x256'  : res_val=256
      endcase
      if res_val ne paths.res_euv then begin & changes+=1 & new_paths.res_euv=res_val & endif
      
       if isa(plot_scl) eq 1 then begin
      if plot_scl[3] ne paths.gridsize then begin & changes+=1 & new_paths.gridsize=plot_scl[3] & endif
      if plot_scl[2] ne paths.cthick then begin & changes+=1 & new_paths.cthick=plot_scl[2] & endif
      case map_identifyer of
        'SOHO': begin
          if plot_scl[0] ne paths.eit_range[0] then begin & changes+=1 & new_paths.eit_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.eit_range[1] then begin & changes+=1 & new_paths.eit_range[1]=plot_scl[1] & endif
          end
        'STEREO': begin
          if plot_scl[0] ne paths.stereo_range[0] then begin & changes+=1 & new_paths.stereo_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.stereo_range[1] then begin & changes+=1 & new_paths.stereo_range[1]=plot_scl[1] & endif
          end
        'SDO': begin
          if plot_scl[0] ne paths.aia_range[0] then begin & changes+=1 & new_paths.aia_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.aia_range[1] then begin & changes+=1 & new_paths.aia_range[1]=plot_scl[1] & endif
          end
      endcase
      endif
      
        if changes gt 0 then begin
          write_ini_catch, dir+'config_CATCH.ini', new_paths,ids.ex_main, /struct_old
    if  systim(/sec)-file_modtime(dir+'/config_CATCH.ini') lt 10 then  paths=new_paths
        endif     
      
      ENDIF
      
      WIDGET_CONTROL, ids.ex_main,/destroy & widget_control, id.main, SENSITIVE=1 & end
    'exit': begin 
      IF paths.lock eq 'off' then begin
        new_paths=paths
        changes=0
        widget_control, ids.ld_path, get_value=temp_path
        if temp_path ne paths.euvpath then begin & changes+=1 & new_paths.euvpath=temp_path & endif
        widget_control, ids.sv_path, get_value=temp_path
        if temp_path ne paths.outpath then begin & changes+=1 & new_paths.outpath=temp_path & endif
        widget_control, ids.lbc, get_value=lbc_val
        if lbc_val eq 'LBC off' then lbc_val='off'
        if lbc_val eq 'LBC on' then lbc_val='on'
        if lbc_val ne paths.lbc then begin & changes+=1 & new_paths.lbc=lbc_val & endif
      resol=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      case resol of
          '4096x4096': res_val=4096
          '2048x2048': res_val=2048
          '1024x1024': res_val=1024
          '512x512'  : res_val=512
          '256x256'  : res_val=256
        endcase
        if res_val ne paths.res_euv then begin & changes+=1 & new_paths.res_euv=res_val & endif

       if isa(plot_scl) eq 1 then begin
      if plot_scl[3] ne paths.gridsize then begin & changes+=1 & new_paths.gridsize=plot_scl[3] & endif
      if plot_scl[2] ne paths.cthick then begin & changes+=1 & new_paths.cthick=plot_scl[2] & endif
      case map_identifyer of
        'SOHO': begin
          if plot_scl[0] ne paths.eit_range[0] then begin & changes+=1 & new_paths.eit_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.eit_range[1] then begin & changes+=1 & new_paths.eit_range[1]=plot_scl[1] & endif
          end
        'STEREO': begin
          if plot_scl[0] ne paths.stereo_range[0] then begin & changes+=1 & new_paths.stereo_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.stereo_range[1] then begin & changes+=1 & new_paths.stereo_range[1]=plot_scl[1] & endif
          end
        'SDO': begin
          if plot_scl[0] ne paths.aia_range[0] then begin & changes+=1 & new_paths.aia_range[0]=plot_scl[0] & endif
          if plot_scl[1] ne paths.aia_range[1] then begin & changes+=1 & new_paths.aia_range[1]=plot_scl[1] & endif
          end
      endcase
      endif

        if changes gt 0 then begin
          write_ini_catch, dir+'config_CATCH.ini', new_paths,ids.ex_main, /struct_old
         if  systim(/sec)-file_modtime(dir+'/config_CATCH.ini') lt 10 then  paths=new_paths
        endif

      ENDIF
       WIDGET_CONTROL, ids.ex_main,/destroy & WIDGET_CONTROL, id.main,/destroy & end
    else: dummy=0
  ENDCASE 

END

PRO insi_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common insitu, insi_ids, date_map, date_plasma, speed, density, temp, date_mag, btsc, bx,by,bz,ranges
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  
  widget_control, ev.id, get_uvalue=uvalue

  CASE uvalue OF
    's_time':begin
      
      widget_control, insi_ids.start_time_slider, get_value=stim_val

      stim= anytim(anytim(date_map)-5.*24.*3600.+ float(stim_val)*3600.,/ccsds)
      
      
      widget_control, insi_ids.start_time, set_value=stim
      widget_control, insi_ids.end_time, get_value=etim
      if anytim(stim) ge (anytim(etim)-24.*3600.) then begin
        etim=anytim(anytim(stim)+24.*3600.,/ccsds)
        widget_control, insi_ids.end_time, set_value=etim
        etim_val=(anytim(etim)-anytim(date_map))/3600.
        widget_control, insi_ids.end_time_slider, set_value=etim_val
      endif
      
      widget_control, insi_ids.mark_slider, get_value=slider_val      &  tr=(anytim(etim)-anytim(stim)) & lineval=anytim(anytim(stim)+(tr*float(slider_val))/1e3,/ccsds)
      widget_control, insi_ids.lab_current, set_value=strmid(lineval,0,19)
      
WSET, insi_ids.draw_insitu1ID
      !p.color=cgcolor('black')
      utplot, date_plasma, speed, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[0],ranges[1]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[0],ranges[1]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[0],ranges[1]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu2ID
      !p.color=cgcolor('black')
      utplot, date_plasma, density, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[2],ranges[3]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[2],ranges[3]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[2],ranges[3]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu3ID
      !p.color=cgcolor('black')
      utplot, date_plasma, temp/1e5, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[4],ranges[5]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[4],ranges[5]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[4],ranges[5]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu4ID
      !p.color=cgcolor('black')
      utplot, date_mag, btsc, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[6],ranges[7]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[6],ranges[7]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[6],ranges[7]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu5ID
      !p.color=cgcolor('black')
      utplot, date_mag, bx, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[8],ranges[9]], timerange=[stim,etim],/nodata
      outplot, [stim,etim], [0,0], color=cgcolor('dark gray'), thick=1.5, linestyle=2
      outplot, date_mag, bx, color=cgcolor('red'), thick=1.5
      outplot, date_mag, by, color=cgcolor('green'), thick=1.5
      outplot, date_mag, bz, color=cgcolor('blue'), thick=1.5
      outplot, [lineval,lineval],[ranges[8],ranges[9]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[8],ranges[9]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      if map_identifyer eq 'STEREO' then begin
        xyouts, 0.85,0.87,'B!Dr!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
        xyouts, 0.9,0.87,'B!Dt!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
        xyouts, 0.95,0.87,'B!Dn!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endif else begin
      xyouts, 0.85,0.87,'B!Dx!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
      xyouts, 0.9,0.87,'B!Dy!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
      xyouts, 0.95,0.87,'B!Dz!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endelse
      
      WSET, insi_ids.draw_insitu6ID
      !p.color=cgcolor('black')
      rndvec=randomu(seed, n_elements(date_mag))
      utplot, date_mag, rndvec, xtit='Time (Starting '+stim+' )', ytit=' ', title=' ', position=[0.08,0.7,0.99,0.99],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=4,charsize=1, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[0,1], timerange=[stim,etim], /nodata,xticklen=!D.y_VSIZE/100., xminor=4
      outplot, [lineval,lineval],[-2,1], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[-2,1], color=cgcolor('blue'), thick=1, linestyle=2
      
      end
    'e_time':begin
              
      widget_control, insi_ids.end_time_slider, get_value=etim_val
      
      etim= anytim(anytim(date_map)+ etim_val*3600.,/ccsds)
      
      widget_control, insi_ids.start_time, get_value=stim
      widget_control, insi_ids.end_time, set_value=etim
      if anytim(etim) le (anytim(stim)+24.*3600.) then begin
        stim=anytim(anytim(etim)-24.*3600.,/ccsds)
        widget_control, insi_ids.start_time, set_value=stim
        stim_val=(anytim(stim)-(anytim(date_map)-24.*5.*3600.))/3600.
        widget_control, insi_ids.start_time_slider, set_value=stim_val
      endif
      
      widget_control, insi_ids.mark_slider, get_value=slider_val      &  tr=(anytim(etim)-anytim(stim)) & lineval=anytim(anytim(stim)+(tr*float(slider_val))/1e3,/ccsds)
      widget_control, insi_ids.lab_current, set_value=strmid(lineval,0,19)
      
WSET, insi_ids.draw_insitu1ID
      !p.color=cgcolor('black')
      utplot, date_plasma, speed, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[0],ranges[1]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[0],ranges[1]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[0],ranges[1]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu2ID
      !p.color=cgcolor('black')
      utplot, date_plasma, density, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[2],ranges[3]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[2],ranges[3]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[2],ranges[3]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu3ID
      !p.color=cgcolor('black')
      utplot, date_plasma, temp/1e5, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[4],ranges[5]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[4],ranges[5]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[4],ranges[5]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu4ID
      !p.color=cgcolor('black')
      utplot, date_mag, btsc, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[6],ranges[7]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[6],ranges[7]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[6],ranges[7]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu5ID
      !p.color=cgcolor('black')
      utplot, date_mag, bx, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[8],ranges[9]], timerange=[stim,etim],/nodata
      outplot, [stim,etim], [0,0], color=cgcolor('dark gray'), thick=1.5, linestyle=2
      outplot, date_mag, bx, color=cgcolor('red'), thick=1.5
      outplot, date_mag, by, color=cgcolor('green'), thick=1.5
      outplot, date_mag, bz, color=cgcolor('blue'), thick=1.5
      outplot, [lineval,lineval],[ranges[8],ranges[9]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[8],ranges[9]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      if map_identifyer eq 'STEREO' then begin
        xyouts, 0.85,0.87,'B!Dr!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
        xyouts, 0.9,0.87,'B!Dt!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
        xyouts, 0.95,0.87,'B!Dn!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endif else begin
      xyouts, 0.85,0.87,'B!Dx!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
      xyouts, 0.9,0.87,'B!Dy!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
      xyouts, 0.95,0.87,'B!Dz!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endelse
      
      WSET, insi_ids.draw_insitu6ID
      !p.color=cgcolor('black')
      rndvec=randomu(seed, n_elements(date_mag))
      utplot, date_mag, rndvec, xtit='Time (Starting '+stim+' )', ytit=' ', title=' ', position=[0.08,0.7,0.99,0.99],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=4,charsize=1, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[0,1], timerange=[stim,etim], /nodata,xticklen=!D.y_VSIZE/100., xminor=4
      outplot, [lineval,lineval],[-2,1], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[-2,1], color=cgcolor('blue'), thick=1, linestyle=2
      
      end
    'mark':begin
      widget_control,/hourglass
      widget_control, insi_ids.start_time, get_value=stim
      widget_control, insi_ids.end_time, get_value=etim
      widget_control, insi_ids.mark_slider, get_value=slider_val      &  tr=(anytim(etim)-anytim(stim)) & lineval=anytim(anytim(stim)+(tr*float(slider_val))/1e3,/ccsds)
      widget_control, insi_ids.lab_current, set_value=strmid(lineval,0,19)
      
WSET, insi_ids.draw_insitu1ID
      !p.color=cgcolor('black')
      utplot, date_plasma, speed, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[0],ranges[1]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[0],ranges[1]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[0],ranges[1]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu2ID
      !p.color=cgcolor('black')
      utplot, date_plasma, density, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[2],ranges[3]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[2],ranges[3]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[2],ranges[3]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu3ID
      !p.color=cgcolor('black')
      utplot, date_plasma, temp/1e5, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[4],ranges[5]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[4],ranges[5]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[4],ranges[5]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu4ID
      !p.color=cgcolor('black')
      utplot, date_mag, btsc, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[6],ranges[7]], timerange=[stim,etim]
      outplot, [lineval,lineval],[ranges[6],ranges[7]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[6],ranges[7]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      WSET, insi_ids.draw_insitu5ID
      !p.color=cgcolor('black')
      utplot, date_mag, bx, xtit=' ', ytit=' ', title=' ', position=[0.08,0.01,0.99,0.95],  Color=cgColor('black'), Background=cgColor('white'), yminor=6,xticklen=!D.y_VSIZE/700., $;psym=10,
        xstyle=9, ystyle=9,charsize=1.2, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[ranges[8],ranges[9]], timerange=[stim,etim],/nodata
      outplot, [stim,etim], [0,0], color=cgcolor('dark gray'), thick=1.5, linestyle=2
      outplot, date_mag, bx, color=cgcolor('red'), thick=1.5
      outplot, date_mag, by, color=cgcolor('green'), thick=1.5
      outplot, date_mag, bz, color=cgcolor('blue'), thick=1.5
      outplot, [lineval,lineval],[ranges[8],ranges[9]*1.5], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[ranges[8],ranges[9]*1.5], color=cgcolor('blue'), thick=1, linestyle=2
      
      if map_identifyer eq 'STEREO' then begin
        xyouts, 0.85,0.87,'B!Dr!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
        xyouts, 0.9,0.87,'B!Dt!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
        xyouts, 0.95,0.87,'B!Dn!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endif else begin
      xyouts, 0.85,0.87,'B!Dx!n', charsize=1, charthick=1.5, color=cgcolor('red'), alignment=0,/norm
      xyouts, 0.9,0.87,'B!Dy!n', charsize=1, charthick=1.5, color=cgcolor('green'), alignment=0,/norm
      xyouts, 0.95,0.87,'B!Dz!n', charsize=1, charthick=1.5, color=cgcolor('blue'), alignment=0,/norm
      endelse
      
      WSET, insi_ids.draw_insitu6ID
      !p.color=cgcolor('black')
      rndvec=randomu(seed, n_elements(date_mag))
      utplot, date_mag, rndvec, xtit='Time (Starting '+stim+' )', ytit=' ', title=' ', position=[0.08,0.7,0.99,0.99],  Color=cgColor('black'), Background=cgColor('white'), $;psym=10,
        xstyle=9, ystyle=4,charsize=1, xmargin=[10,10], ymargin=[10,10], xthick=1.5, ythick=1.5,charthick=1.5, thick=1.5, yrange=[0,1], timerange=[stim,etim], /nodata,xticklen=!D.y_VSIZE/100., xminor=4
      outplot, [lineval,lineval],[-2,1], color=cgcolor('red'), thick=1
      outplot, [date_map,date_map],[-2,1], color=cgcolor('blue'), thick=1, linestyle=2
      
      end
    'abort_insi': WIDGET_CONTROL, insi_ids.insi_main,/destroy
    'save_insi': begin
      widget_control,/hourglass
      
      x_size=21.0       ;x size in cm
      y_size=29.7     ;x size in cm
      x_offset=0
      y_offset=1

      lin_thick=2
      ;---------------------------------------- Data ----------------------------------------
      ;timerange
      widget_control, insi_ids.start_time, get_value=stim
      widget_control, insi_ids.end_time, get_value=etim
      xr=[stim,etim]


      if map_identifyer eq 'SOHO' or map_identifyer eq 'SDO' then begin & obs= 'ACE' & endif else begin
        if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then obs= map_identifyer+'_A'
        if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then obs= map_identifyer+'_B'
      endelse
      
      ;title
      tit='In-Situ Measurements '+obs

      cloudstart=[date_map,date_map]

      ;panel 1:         density
      yr1=[ranges[2],ranges[3]]
      y1=density
      ytit1='N!Dp!N [cm!U-3!N]'
      x1=date_plasma

      ;panel 2:         velocity
      yr2=[ranges[0],ranges[1]]
      y2=speed
      ytit2='v!Dp!N [km s!U-1!N]'
      x2=date_plasma

      ;panel 3:         temperature
      yr3=[ranges[4],ranges[5]]
      y3=temp/1e5
      ytit3='T!Dp!N [10!U5!N K]
      x3=date_plasma
      ;----------------------
      ;expected temp
      h500= where(y2 gt 500)
      l500= where(y2 le 500)
      y3_exp=y2
      ;dis=SATELLITE_HCI_5m.radius[*]/double(149578710)
      ;y4_exp[l500]=(((0.0106*y1[l500] - 0.278)*3.)/dis)/1e2
      ;y4_exp[h500]=((0.77*y1[h500] - 265.)/dis)/1e2

      y3_exp[h500]=(0.77*y2[h500]-265)/1e2
      y3_exp[l500]=((0.031*y2[l500]-4.39)^2.)/1e2
      ;------------------------------------------------
      ;

      ;panel 5-8:         magnetic field
      yr6=[ranges[8],ranges[9]]
      yr6_abs=[ranges[6],ranges[7]]
      y6=btsc
      y6_x=bx
      y6_y=by
      y6_z= bz
      ytit6='B [nT]'
      
      if map_identifyer eq 'STEREO' then begin
        ytit6_x='B!Dr!N [nT]'
        ytit6_y='B!Dt!N [nT]'
        ytit6_z='B!Dn!N [nT]'
      endif else begin
        ytit6_x='B!Dx!N [nT]'
        ytit6_y='B!Dy!N [nT]'
        ytit6_z='B!Dz!N [nT]'
      endelse
      
      x6=date_mag
      x6x=date_mag
      x6y=date_mag
      x6z=date_mag


      widget_control, ids.sv_path, get_value=temp_path
      
      outname=temp_path+strmid(windex.date_obs,0,16)+'_'+obs+'_in_situ_plot.eps'
      
      if file_test(outname) eq 1 then begin
        res=dialog_message( ['                                    ',$
          '       File already exists!         ',$
          '     Do you want to overwrite!      ',$
          '                                    '], dialog_parent=ids.ex_main,/question)
        if res eq 'No' then begin
          file_number=1
          while file_test(outname) eq 1 do begin
            file_number++
            outname=temp_path+strmid(windex.date_obs,0,16)+'_'+obs+'_in_situ_plot_'+strtrim(string(file_number),2)+'.eps'
          endwhile
        endif

      endif
      
      
      SET_PLOT, 'PS'
      device, filename=outname,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0
      !p.multi=[0,20,1]

      xticklength=0.08
      

      ;panel 1
      pn=1
      utplot, x1, y1,  ytit=' ' , title=tit, thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr1, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr1, linestyle=0, color =cgcolor('dodger blue'),thick=3


      utplot, x1, y1,  ytit=ytit1 , title=tit, thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr1, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength
      

      outplot, x1, y1, linestyle=0, color =cgcolor('black'),thick=lin_thick

      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      axis,  xaxis=1,/nodata, xthick=3,XTICKFORMAT="(A1)",xticklen=0.00005;xticklength

      ;panel 2
      pn=2
      utplot, x2, y2, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr2, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr2, linestyle=0, color =cgcolor('dodger blue'),thick=3

      utplot, x2, y2, ytit=ytit2 , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr2, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot, x2, y2, linestyle=0, color =cgcolor('black'),thick=lin_thick

      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1


      ;panel 3
      pn=3
      utplot, x3, y3, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr3, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr3, linestyle=0, color =cgcolor('dodger blue'),thick=3

      utplot, x3, y3, ytit=ytit3 , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr3, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength
      

      outplot, x3, y3, linestyle=0, color =cgcolor('black'),thick=lin_thick

      outplot, x2, y3_exp,linestyle=0, color =cgcolor('red'),thick=lin_thick+1

      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      xyouts, 0.87,  0.91-0.135*(pn-0.7),'T!Dexp!N',size=1.3, charthick=3.5, color=cgcolor('red'),/NORMAL, alignment=1

      ;panel 4
      pn=4
      
      utplot, x6, y6, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6_abs, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength
      
      outplot,cloudstart,yr6_abs, linestyle=0, color =cgcolor('dodger blue'),thick=3
      
      utplot, x6, y6, ytit=ytit6 , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
      xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6_abs, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength
      
      outplot, x6,y6, linestyle=0, color =cgcolor('black'),thick=lin_thick
      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      

      ;panel 5
      pn=5
      
      utplot, x6, y6_x, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr6, linestyle=0, color =cgcolor('dodger blue'),thick=3

      utplot, x6, y6_x, ytit=ytit6_x , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength
      
      outplot,[x6[0],x6[-1]],[0,0], linestyle=1, color =cgcolor('dark gray'),thick=2
      outplot, x6,y6_x, linestyle=0, color =cgcolor('black'),thick=lin_thick
      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      
      ;panel 6
      pn=6

      utplot, x6, y6_y, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr6, linestyle=0, color =cgcolor('dodger blue'),thick=3

      utplot, x6, y6_y, ytit=ytit6_y , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,[x6[0],x6[-1]],[0,0], linestyle=1, color =cgcolor('dark gray'),thick=2
      outplot, x6,y6_y, linestyle=0, color =cgcolor('black'),thick=lin_thick
      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      
      ;panel 7
      pn=7

      utplot, x6, y6_z, ytit=' ' , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata,XTICKFORMAT="(A1)", xtit=' ',yminor=1,xticklen=xticklength

      outplot,cloudstart,yr6, linestyle=0, color =cgcolor('dodger blue'),thick=3

      utplot, x6, y6_z, ytit=ytit6_z , title=' ', thick=6,position=[0.1,0.94-0.12*pn,0.9,0.94-0.12*(pn-1)],timerange =timerange, color=cgcolor('black'),$
        xstyle=9, ystyle=9, xmargin=[10,10], ymargin=[10,10], yrange=yr6, xthick=3, ythick=3,charthick=3,charsize=2,/nodata, xtit='Time after ' + strmid(strtrim(string(stim),2),0,16),yminor=1,xticklen=xticklength

      outplot,[x6[0],x6[-1]],[0,0], linestyle=1, color =cgcolor('dark gray'),thick=2
      outplot, x6,y6_z, linestyle=0, color =cgcolor('black'),thick=lin_thick
      axis,  yaxis=1,/nodata, yticklen=0.0001,ythick=3,yTICKFORMAT="(A1)",yminor=1
      

      clear_utplot
      DEVICE, /CLOSE_FILE
      SET_PLOT, 'X'
      !p.multi=0
      
      res=dialog_message('In-Situ plot saved!', dialog_parent=insi_ids.insi_main)
      
      end
  
  endcase
end

PRO opt_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  common dtool, boundary_exists, draw_binmap,dcord2,xcord2,ycord2,draw_dex,mousebutton

  widget_control, ev.id, get_uvalue=uvalue
  CASE uvalue OF
'dmin_text':begin
              if ev.Type LE 1 then begin
              Altered = 0
              ;print,ev.Offset,ev.Length
              WIDGET_CONTROL, ids.dmin_text, $
                SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
              WIDGET_CONTROL, ids.dmin_text, SET_VALUE='', /USE_TEXT_SELECT
                if ev.type eq 0 then begin
                  FIELD_INT, ev.Ch ,ev, altered ,ids.dmin_text
                endif else begin
                  Chars= BYTE(ev.Str) 
                  FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,ids.dmin_text
                endelse
              endif ELSE begin
              endelse
              ;   All delete/add char evs effect the contents of
              ;   a string. <CR> is considered special.
              IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
              IF ev.Type EQ 0 THEN $
                Altered  = 1 + (ev.Ch EQ 10b)
            end
'dmax_text':begin
              if ev.Type LE 1 then begin
                Altered = 0
                ;print,ev.Offset,ev.Length
                WIDGET_CONTROL, ids.dmax_text, $
                  SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
                WIDGET_CONTROL, ids.dmax_text, SET_VALUE='', /USE_TEXT_SELECT
                if ev.type eq 0 then begin
                  FIELD_INT, ev.Ch ,ev, altered,ids.dmax_text
                endif else begin
                  Chars= BYTE(ev.Str)
                  FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,ids.dmax_text
                endelse
              endif ELSE begin
              endelse
              ;   All delete/add char evs effect the contents of
              ;   a string. <CR> is considered special.
              IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
              IF ev.Type EQ 0 THEN $
                Altered  = 1 + (ev.Ch EQ 10b)
            end
'grid_text':begin
              if ev.Type LE 1 then begin
                Altered = 0
                ;print,ev.Offset,ev.Length
                WIDGET_CONTROL, ids.grid_text, $
                  SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
                WIDGET_CONTROL, ids.grid_text, SET_VALUE='', /USE_TEXT_SELECT
                if ev.type eq 0 then begin
                  FIELD_INT, ev.Ch ,ev, altered,ids.grid_text
                endif else begin
                  Chars= BYTE(ev.Str)
                  FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,ids.grid_text
                endelse
              endif ELSE begin
              endelse
              ;   All delete/add char evs effect the contents of
              ;   a string. <CR> is considered special.
              IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
              IF ev.Type EQ 0 THEN $
                Altered  = 1 + (ev.Ch EQ 10b)
            end
'thick_text':begin
              if ev.Type LE 1 then begin
                Altered = 0
                ;print,ev.Offset,ev.Length
                WIDGET_CONTROL, ids.thick_text, $
                  SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
                WIDGET_CONTROL, ids.thick_text, SET_VALUE='', /USE_TEXT_SELECT
                if ev.type eq 0 then begin
                  FIELD_INT, ev.Ch ,ev, altered,ids.thick_text
                endif else begin
                  Chars= BYTE(ev.Str)
                  FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,ids.thick_text
                endelse
              endif ELSE begin
              endelse
              ;   All delete/add char evs effect the contents of
              ;   a string. <CR> is considered special.
              IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
              IF ev.Type EQ 0 THEN $
                Altered  = 1 + (ev.Ch EQ 10b)
            end
'abort_opt': WIDGET_CONTROL, ids.opt_main,/destroy;begin &  &  widget_control, ids.ex_main, sensitive=1 & end
'ap_opt': begin
  widget_control, /hourglass
  widget_control, ids.dmin_text, get_value=ndmin
  widget_control, ids.dmax_text, get_value=ndmax
  widget_control, ids.grid_text, get_value=ngrid
  
  if ids.thick_text ne long(0) then  begin & widget_control, ids.thick_text, get_value=nthick & plot_scl[2]=fix(nthick) & endif
  plot_scl[0]=fix(ndmin) &  plot_scl[1]=fix(ndmax) & plot_scl[3]=fix(ngrid)
  
 
     ixixdate=anytim(themap.time,/ccsds)
     pmap=themap
     case map_identifyer of
       'SOHO': eit_colors, 195,/silent
       'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
       'SDO': aia_lct, wave='193', /load
     endcase

widget_control,ids.opt_main, get_uvalue=uval

  if uval eq 'poptdraw' then begin
     WSET, ids.draw_bigID
     ixixdate=anytim(themap.time,/ccsds)
     plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255
     dcord2=!D & xcord2=!X & ycord2=!Y
     
  endif else begin
  
  WSET, ids.drawID
  plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1],log=1, grid_spacing=plot_scl[3] 
  xyouts, -1070,-1070,strmid(ixixdate,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5
  dcord=!D & xcord=!X & ycord=!Y

  widget_control, ids.popt, sensitive=1
  widget_control, ids.draww,sensitive=1
  widget_control, ids.save,sensitive=0
  
  boundary_exists=0
  extracted=0
  
  endelse
  
  WIDGET_CONTROL, ids.opt_main,/destroy
  
  end
  endcase
end


PRO drawi_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  common dtool, boundary_exists, draw_binmap,dcord2,xcord2,ycord2,draw_dex,mousebutton

  widget_control, ev.id, get_uvalue=uvalue
  
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN
    IF (ev.RELEASE eq 1) THEN BEGIN
    mousebutton = 0
    widget_control, ids.draw_erase, get_value=draw_erase_val         
      if draw_erase_val ne 'Drawing Mode' then begin
             
              pmap=themap
          case map_identifyer of
            'SOHO': eit_colors, 195,/silent
            'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
            'SDO': aia_lct, wave='193', /load
          endcase
    
      WSET, ids.draw_bigID
      ixixdate=anytim(themap.time,/ccsds)
      plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255

      plot_map, draw_binmap, /over, /cont, color=cgcolor('red'),/overlay, contour=1,/cell_fill,levels=[1]
      
      dcord2=!D & xcord2=!X & ycord2=!Y
    endif
    ENDIF
    IF ev.press eq 1 THEN mousebutton = 1
    IF mousebutton eq 1  then begin
      
    t0=systim(/sec)
        Dx = (double(ev.x) / dcord2.X_VSIZE - xcord2.s[0]) / xcord2.s[1]
        DY = (double(ev.Y) / dcord2.y_VSIZE - Ycord2.s[0]) / ycord2.s[1]
        crd=[Dx,Dy]
        p_sel=[crd[0]/draw_dex.cdelt1+draw_dex.crpix1,crd[1]/draw_dex.cdelt2+draw_dex.crpix2]
        
        widget_control, ids.draw_radius, get_value=brushsize       
        dist_circle, circ_struct, n_elements(draw_binmap.data[*,0]),p_sel[0],p_sel[1]
 
        widget_control, ids.draw_erase, get_value=draw_erase_val
        
        c_struct=circ_struct & c_struct[*]=0
        c_struct[where(circ_struct le brushsize)]=1
        brushdex=where(c_struct eq 1)
 
        
        if draw_erase_val eq 'Drawing Mode' then begin & draw_binmap.data[brushdex]=1 & drawcol='red' & endif else begin & draw_binmap.data[brushdex]=0 & drawcol='blue' & endelse             
        
              plot_binmap=draw_binmap & plot_binmap.data=c_struct
        
        plot_map, plot_binmap, /over, /cont, color=cgcolor(drawcol),/overlay, contour=1,levels=[1],/cell_fill

        
        dcord2=!D & xcord2=!X & ycord2=!Y  
    ENDIF
  ENDIF

  
  CASE uvalue OF
    'brush_draw':
    'draw_big':
    'popt_draw':begin
      widget_control, ids.draw_main, TLB_GET_OFFSET=offs

      opt_main = widget_base(title='Options', xsize=170, ysize=220-40,SCR_XSIZE=170,SCR_YSIZE=220-40,modal=(1-debug), group_leader=ids.draw_main, xoffset=offs[0]+439, yoffset=offs[1]+284, uvalue='poptdraw')

      top_opt = Widget_Base(opt_main, XOFFSET=10)
      label = Widget_Label(top_opt, value='Plot Options', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      opt = Widget_Base(top_opt, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=200-40,xsize=150)
      label = Widget_Label(top_opt, value='Plot Options', XOFFSET=10)


      label_dmin = WIDGET_LABEL(opt, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text=widget_TEXT(opt,value=strtrim(string(plot_scl[0],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=10, ysize=1, uvaL='dmin_text', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(opt, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text=widget_TEXT(opt,value=strtrim(string(plot_scl[1],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=50, ysize=1, uvaL='dmax_text', units=0,/editable,/ALL_EVENTS)
      label_grid = WIDGET_LABEL(opt, XSIZE=50, VALUE='Grid:', xoffset=10, yoffset=95, /align_left)
      grid_text=widget_TEXT(opt,value=strtrim(string(plot_scl[3],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=90, ysize=1, uvaL='grid_text', units=0,/editable,/ALL_EVENTS)
      ;label_thick = WIDGET_LABEL(opt, XSIZE=50, VALUE='Thick:', xoffset=10, yoffset=135, /align_left)
      ;thick_text=widget_TEXT(opt,value=strtrim(string(plot_scl[2],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=135, ysize=1, uvaL='thick_text', units=0,/editable,/ALL_EVENTS)

      abort_opt= widget_button(opt,value='Abort', uval='abort_opt',/NO_release, xsize=60 , xoffset=10., yoffset=130, tooltip='Close options window',ysize=25)
      apply_opt= widget_button(opt,value='Apply', uval='ap_opt',/NO_release, xsize=60 , xoffset=80., yoffset=130, tooltip='Apply options',ysize=25)

      ids.opt_main=opt_main & ids.dmin_text=dmin_text & ids.grid_text=grid_text
      ids.dmax_text=dmax_text & ids.apply_opt=apply_opt & ids.abort_opt=abort_opt ;& ids.thick_text=thick_text


      widget_control, opt_main, /realize

      xmanager, 'opt',opt_main,  /no_block
    end
    'new_draw': begin
      pmap=themap
      case map_identifyer of
        'SOHO': eit_colors, 195,/silent
        'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
        'SDO': aia_lct, wave='193', /load
      endcase

      WSET, ids.draw_bigID
      ixixdate=anytim(themap.time,/ccsds)
      plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255

      draw_binmap.data[*]=0

      dcord2=!D & xcord2=!X & ycord2=!Y
      end
    'morph_draw':
    'activate_morph':begin
      widget_control, ids.draw_morph, get_value=morph_value
      
      if morph_value ne 0 then  begin
        dist_circle, circ_struct, 30
        c_struct=circ_struct &  c_struct[*]=1
        c_struct[where(circ_struct gt morph_value)]=0

          draw_binmap.data=morph_open(draw_binmap.data,c_struct)
          draw_binmap.data=morph_close(draw_binmap.data,c_struct)

          pmap=themap
          case map_identifyer of
            'SOHO': eit_colors, 195,/silent
            'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
            'SDO': aia_lct, wave='193', /load
          endcase

          WSET, ids.draw_bigID
          ixixdate=anytim(themap.time,/ccsds)
          plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255

          plot_map, draw_binmap, /over, /cont, color=cgcolor('red'),/overlay, contour=1,/cell_fill,levels=[1]

          dcord2=!D & xcord2=!X & ycord2=!Y
      endif
      
      end
    'toggle_draw':begin &  widget_control, ev.id, get_value=value   & if value eq 'Drawing Mode' then widget_control, ev.id, set_value='Eraser Mode' else widget_control, ev.id, set_value='Drawing Mode' &  end
    'abort_draw': WIDGET_CONTROL, ids.draw_main,/destroy;begin &  &  widget_control, ids.ex_main, sensitive=1 & end
    'ap_draw': begin
      widget_control, /hourglass
          i=0
          chmap=rem_map_limb(draw_binmap, missing = !values.f_nan)
          chmaps=replicate(chmap,5)
          ch_prop=make_array(7,11); binmaps1-5, average, stddev;area, meanint, medianint, com_lng, com_lat, com_xc, com_yc, lng_min, lng_max, lat_min, lat_max
          meanint_prop=make_array(5,3)

            helpmap=chmap    &  helpmap.data[where(chmap.data eq 0)]=!values.f_nan
            curve_corr, helpmap, corr_map,coreg=coreg,area=area

            if map_identifyer eq 'SOHO' then begin & mc_obs='SOHO' & endif else begin
              if map_identifyer eq 'SDO'  then begin & mc_obs='SDO' & endif else begin
                if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then mc_obs='STEREO_A'
                if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then mc_obs='STEREO_B'
              endelse
            endelse

            calc_masscenter, corr_map, xy_center,coord=coord, coreg = coreg, observer=mc_obs

            ch_prop[i,0]=area
            ch_prop[i,1]=median(workmap.data[where(chmap.data eq 1)])
            meanint_mom=(moment(workmap.data[where(chmap.data eq 1)],/nan,maxmoment=2))
            ch_prop[i,2]=meanint_mom[0]
            ch_prop[i,3]=xy_center[1] &  ch_prop[i,4]=xy_center[0]
            ch_prop[i,5]=coord[0] & ch_prop[i,6]=coord[1]

            mcrpix1=comp_fits_crpix(chmap.xc,chmap.dx,n_elements(chmap.data[*,0]),0.)
            mcrpix2=comp_fits_crpix(chmap.yc,chmap.dy,n_elements(chmap.data[0,*]),0.)
            mapdate=anytim(chmaps[i].time,/ccsds)
            
            ind = ARRAY_INDICES(chmap.data, where(chmap.data eq 1))
            ind_coord = [ ( ind[0,*] - (mcrpix1)) * chmap.dx , (ind[1,*] - (mcrpix2 )) * chmap.dy ]
            if map_identifyer eq 'SOHO' then begin & ang=pb0r(workmap.time, /soho, l0=l0,/arcsec) & endif else begin
              if map_identifyer eq 'SDO'  then begin & ang=pb0r(workmap.time, /earth, l0=l0,/arcsec) & endif else begin
                if strmatch(workmap.id,'*STEREO_A*', /fold_case) eq 1 then ang=pb0r(workmap.time, /sta, l0=l0,/arcsec)
                if strmatch(workmap.id,'*STEREO_B*', /fold_case) eq 1 then ang=pb0r(workmap.time, /stb, l0=l0,/arcsec)
              endelse
            endelse

            p=ang[0] & b0=ang[1] & r=ang[2]
            lonlat_coord = arcmin2hel(ind_coord[0,*]/60, ind_coord[1,*]/60, date=mapdate, b0=b0,l0=l0,rsun=r,p=p)

            minmax_lng_px=minmax(lonlat_coord[1,*],/nan)
            minmax_lat_px=minmax(lonlat_coord[0,*],/nan)

            ch_prop[i,7]=minmax_lng_px[0] &    ch_prop[i,8]=minmax_lng_px[1]
            ch_prop[i,9]=minmax_lat_px[0] &    ch_prop[i,10]=minmax_lat_px[1]
   

          for j=0, 10 do begin & ch_prop[5,j]=ch_prop[0,j] & ch_prop[6,j]=!values.f_nan & endfor

          WSET, ids.drawID

          pmap=themap
          case map_identifyer of
            'SOHO': eit_colors, 195,/silent
            'STEREO': SECCHI_COLORS, 'EUVI', 195, /LOAD
            'SDO': aia_lct, wave='193', /load
          endcase


          ixixdate=anytim(themap.time,/ccsds)

          plot_map, pmap, xrange=[-1100,1100],yrange=[-1100,1100],dmin=plot_scl[0],dmax=plot_scl[1],/log, title=strmid(ixixdate,0,16),window=!D.Window,  xstyle=5, ystyle=5, position=[0,0,1,1], grid_spacing=plot_scl[3];, g_color=255
          plot_map, chmap, /over, /cont, color=cgcolor('red'),c_thick=plot_scl[2], c_linestyle=0
          plots,ch_prop[0,5],ch_prop[0,6], psym=7, color=cgcolor('gold'), symsize=1.5, thick=2.5

          xyouts, -1070,-1070,strmid(ixixdate,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5
          dcord=!D & xcord=!X & ycord=!Y

          if ch_prop[5,7] lt -80 then begin & ch_prop[5,7]=!values.f_nan  & ch_prop[6,7]=!values.f_nan & endif
          if ch_prop[5,8] gt 80 then begin & ch_prop[5,8]=!values.f_nan  & ch_prop[6,8]=!values.f_nan & endif
          if ch_prop[5,9] lt -80 then begin & ch_prop[5,9]=!values.f_nan  & ch_prop[6,9]=!values.f_nan & endif
          if ch_prop[5,10] gt 80 then begin & ch_prop[5,10]=!values.f_nan  & ch_prop[6,10]=!values.f_nan  & endif

          WIDGET_CONTROL, ids.vlabel_area, SET_VALUE=strtrim(STRING(ch_prop[5,0]/1e10, format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_medint, SET_VALUE=strtrim(STRING(ch_prop[5,1], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_meanint, SET_VALUE=strtrim(STRING(ch_prop[5,2], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_lng, SET_VALUE=strtrim(STRING(ch_prop[5,3], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_lat, SET_VALUE=strtrim(STRING(ch_prop[5,4], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_exlng, SET_VALUE=strtrim(STRING(ch_prop[5,7], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[5,8], format='(F15.2)'),2)
          WIDGET_CONTROL, ids.vlabel_exlat, SET_VALUE=strtrim(STRING(ch_prop[5,9], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[5,10], format='(F15.2)'),2)

          area_ratio=!values.f_nan;999;100.*ch_prop[6,0]/ch_prop[5.0]
          WIDGET_CONTROL, ids.vlabel_area_err, SET_VALUE=strtrim(STRING(ch_prop[6,0]/1e10, format='(F15.3)'),2)+' | '+strtrim(STRING(area_ratio, format='(F15.1)'),2)+'%'
          WIDGET_CONTROL, ids.vlabel_medint_err, SET_VALUE=strtrim(STRING(ch_prop[6,1], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_meanint_err, SET_VALUE=strtrim(STRING(ch_prop[6,2], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_lng_err, SET_VALUE=strtrim(STRING(ch_prop[6,3], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_lat_err, SET_VALUE=strtrim(STRING(ch_prop[6,4], format='(F15.3)'),2)
          WIDGET_CONTROL, ids.vlabel_exlng_err, SET_VALUE=strtrim(STRING(ch_prop[6,7], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[6,8], format='(F15.2)'),2)
          WIDGET_CONTROL, ids.vlabel_exlat_err, SET_VALUE=strtrim(STRING(ch_prop[6,9], format='(F15.2)'),2)+' | '+strtrim(STRING(ch_prop[6,10], format='(F15.2)'),2)


          WSET, ids.logoID_traffic_lights
    graycol=194
    lights=make_array(3,16,16) & lights[0,*,*]=graycol & lights[1,*,*]=graycol & lights[2,*,*]=graycol
    tvimage, lights

          extracted=0
          boundary_exists=1
          widget_control, ids.save,sensitive=1

      WIDGET_CONTROL, ids.draw_main,/destroy

    end
  endcase
end


;+
;******************************************************************************************
; NAME: chproperties2txt
;
; Call:
;       chproperties2txt, in, path
;
; PURPOSE:
;        Prints CATCH coronal hole properties to file
;
;
; INPUTS:
;       in               CATCH CH property structure
;       path             Output directory
;
; OUTPUTS:
;       textfile
;
;
; MODIFICATION HISTORY:
;        Written by: Stephan Heinemann, November 16, 2018
;
;*******************************************************************************************
;-
pro chproperties2txt, in,thr_in, path, obs, obs_date
common version, version
close,/all
openw,1,path
  
in[where(finite(in) eq 0)]=9999
in[where(in eq 0)]=9999

printf, 1, ';### Coronal hole properties extracted from EUV ###'
printf, 1, ';### CATCH TOOL Version '+version+' ###'
printf, 1, ';Observer   Date Observed   Threshold [%]   Threshold [DN]   Area [10^10 km^2]    Area Sigma [10^10 km^2]     Mean Intensity [DN]     Mean Intensity Sigma [DN]   Median Intensity [DN]   Median Intensity Sigma [DN]'+$
           '    Center of Mass Longitude [°]   Center of Mass Longitude Sigma [°]   Center of Mass Latitude [°]    Center of Mass Latitude Sigma [°]'+$
           '    Longitudinal Extent (East) [°]   Longitudinal Extent Sigma (East) [°]   Longitudinal Extent (West) [°]   Longitudinal Extent Sigma (West) [°]',$
           '    Latitudinal Extent (South) [°]   Latitudinal Extent Sigma (South) [°]   Latitudinal Extent (North) [°]   Latitudinal Extent Sigma (North) [°]'
printf, 1,   obs+'  '+obs_date+'  '+strtrim(STRING(thr_in[0], format='(I)'),2)+'  '+strtrim(STRING(thr_in[1], format='(I)'),2)+'  '+$
                                    strtrim(STRING(in[5,0]/1e10, format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,0]/1e10, format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,2], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,2], format='(D15.5)'),2)+'  '+$
                                    strtrim(STRING(in[5,1], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,1], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,3], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,3], format='(D15.5)'),2)+'  '+$
                                    strtrim(STRING(in[5,4], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,4], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,7], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,7], format='(D15.5)'),2)+'  '+$
                                    strtrim(STRING(in[5,8], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,8], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,9], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,9], format='(D15.5)'),2)+'  '+$
                                    strtrim(STRING(in[5,10], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,10], format='(D15.5)'),2)
printf, 1, ';##############################'
printf, 1, ';### You may read this with idl using the following ssw routine:'
printf, 1, ';### readcol, file, x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,$'
printf, 1, ';### format='+string(39B)+'(A,A,I,I,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5)'+string(39B)+',STRINGSKIP='+string(39B)+';'+string(39B)
printf, 1, ';##############################'

close, /all
end

;+
;*********************************************************************************
; NAME: FIELD_INT
;
; PURPOSE:
;        enables integer only input for plot options
;
;
; MODIFICATION HISTORY:
;        Written by: Stephan Heinemann, March 5, 2019
;        based on CW_FIELD
;
;*******************************************************************************************
;-
PRO FIELD_INT, Ch,  Event, Altered, Text_ID
  common general, id, paths,dir,debug
  common menu_id, menuid
  common ex, ids, temp_path, themap, workmap, binmap, lfiles, map_identifyer,windex, dcord, xcord,ycord, bmaps, extracted,ch_prop, plot_scl,chmaps,thr_value,reso
  ;COMPILE_OPT hidden

  Altered = 0     ; nothing so far
  Nil     = 0     ; field has contents
  Minus   = 0     ; field is not just a '-'
  Negate  = 0     ; new text has no '-'s in it
  TextId  = Text_ID

  ; Special Cases:
  ;   We don't actually care where in the input string a
  ;   '-' is.  If there is an odd number of them, we
  ;   negate the value (see below)
  ;
  ;   Current String      Char        Result
  ;   Nil         '-'     '-'
  ;   -           '-'     Nil
  ;   <any number>        '-'     -<number>

  WIDGET_CONTROL, TextId, GET_VALUE=Value
  Value   = Value[0]

  IF Value EQ '' THEN Nil = 1     ; Value is nil string
  IF Value EQ '-' THEN Minus = 1  ; Value is an invalid number


  ;   <CR>
  IF Ch EQ 10b THEN BEGIN
    Altered = 2
    RETURN
  ENDIF

  IF Ch EQ 45b THEN Negate = 1 $
  ELSE IF Ch GE 48b AND Ch LE 57b THEN BEGIN
    Nil = 0 & Minus = 0
  ENDIF ELSE RETURN   ; ![0-9]

  ;   Add new character (if any)

  Selection   = WIDGET_INFO(TextId, /TEXT_SELECT)
  TIP     = Selection[0]+1-Negate ; Text Insertion Point

  IF Negate EQ 0 THEN BEGIN
    WIDGET_CONTROL, TextId, SET_VALUE=STRING(Ch), /USE_TEXT_SELECT
    Altered = 1
  ENDIF

  IF Negate THEN BEGIN
    IF Nil THEN BEGIN
      WIDGET_CONTROL, TextId, SET_VALUE='-'
      TIP = 1
    ENDIF ELSE IF Minus THEN BEGIN
      WIDGET_CONTROL, TextId, SET_VALUE=''
    ENDIF ELSE BEGIN
      ;   We actually have a number to negate

      WIDGET_CONTROL, TextId, GET_VALUE=Value
      IValue  = LONG(Value)
      TIP     = TIP + (IValue GT 0) - (IValue LT 0)
      WIDGET_CONTROL, TextId, SET_VALUE=STRTRIM(-IValue,2)
    ENDELSE
    Altered = 1
  ENDIF

  ; Set selection point
  IF Altered THEN WIDGET_CONTROL, TextId, SET_TEXT_SELECT=[TIP,0]
END