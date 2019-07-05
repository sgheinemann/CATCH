;########################### magneti field analysis Widget ####################################
pro mag_widget, ev

  common general, id, paths,dir,debug
  common menu_id, menuid
  common mag, ids, temp_path_bin, temp_path_mag, magmap, binmap, lfiles_bin, lfiles_mag, mag_prop,plot_scl, dcord, xcord,ycord,pmap,binmaps,pindex,bindex,ft_status,binfil,reso,custom
  
  widget_control, id.main, TLB_GET_OFFSET=offs
  
  mag_main = widget_base(title='CATCH: CH Magnetic Field', xsize=1000, ysize=700,SCR_XSIZE=1000,SCR_YSIZE=700,modal=(1-debug), group_leader=id.main, xoffset=offs[0]-170, yoffset=offs[1]-205)
 

  top = Widget_Base(mag_main, XOFFSET=10)
  label = Widget_Label(top, value='Coronal hole photospheric magnetic field analysis', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  mag_base= Widget_Base(top, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=680,xsize=980)
  label = Widget_Label(top, value='Coronal hole photospheric magnetic field analysis', XOFFSET=10)

  draww = widget_draw(mag_base, uvalue='draw',/MOTION_EVENTS,xsize=512, ysize=512, xoffset=980-522, yoffset=10,/frame,/button)

  top2 = Widget_Base(mag_base, XOFFSET=10, yoffset=10)
  label = Widget_Label(top2, value='File Management', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  data_field = Widget_Base(top2, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=440,xsize=428)
  label = Widget_Label(top2, value='File Management', XOFFSET=10)

  sv_path=widget_button(data_field,value='Output Directory:',uval='sv_path', /no_release, xsize=130, xoffset=10, yoffset=20,ysize=25, tooltip='Specify output directory for results')
  sv_path_text=widget_TEXT(data_field,value=paths.outpath, xsize=42, XOFFSET=150,yoffset=17, ysize=1, uvaL='sv_text', units=0,/editable)

  ld_path_mag=widget_button(data_field,value='Data Directory:',uval='ld_path_mag', /no_release, xsize=130, xoffset=10, yoffset=55,ysize=25, tooltip='Specify input directory for magnetic field data')
  ld_path_text_mag=widget_TEXT(data_field,value=paths.magpath, xsize=42, XOFFSET=150,yoffset=52, ysize=1, uvaL='ld_text_mag', units=0,/editable)
  
  label_list_mag = WIDGET_LABEL(data_field, XSIZE=170, VALUE='Magnetograms:', xoffset=10, yoffset=100, /align_left)
  refresh_list_mag=widget_button(data_field,value='Refresh',uval='refresh_mag', /no_release, xsize=100, xoffset=190, yoffset=95,ysize=25, tooltip='Refresh list of input files')

  temp_path_mag=paths.magpath
  lfiles_mag=file_search(temp_path_mag+['*hmi*.fits','*M_96m*.fits'],/fold_case)
  lfiles_mag=file_basename(lfiles_mag)
  files_list_mag=widget_list(data_field,value=lfiles_mag,uval='files_mag', xsize=63, xoffset=10, yoffset=130,ysize=4)

  
  ld_path_bin=widget_button(data_field,value='CATCH Directory:',uval='ld_path_bin', /no_release, xsize=130, xoffset=10, yoffset=230,ysize=25, tooltip='Specify input directory for CH binary map')
  ld_path_text_bin=widget_TEXT(data_field,value=paths.outpath, xsize=42, XOFFSET=150,yoffset=227, ysize=1, uvaL='ld_text_bin', units=0,/editable)

  label_list_bin = WIDGET_LABEL(data_field, XSIZE=170, VALUE='Binary Coronal Hole Maps:', xoffset=10, yoffset=275, /align_left)
  refresh_list_bin=widget_button(data_field,value='Refresh',uval='refresh_bin', /no_release, xsize=100, xoffset=190, yoffset=270,ysize=25, tooltip='Refresh list of input files')
  
  temp_path_bin=paths.outpath
  lfiles_bin=file_search(paths.outpath,'*CATCH_binary_ch_mask*'+'.fits',/fold_case)
  lfiles_bin=file_basename(lfiles_bin)
  files_list_bin=widget_list(data_field,value=lfiles_bin,uval='files_bin', xsize=63, xoffset=10, yoffset=305,ysize=4)
  
  load=widget_button(data_field,value='Load', uval='load',/NO_release, xoffset=318, yoffset=405,xsize=100,ysize=25,tooltip='Load selected files')

  label_reso = WIDGET_LABEL(data_field, XSIZE=150, VALUE='Magnetogram Resolution:', yoffset=410, xoffset=10, /align_left)
  cbox_reso=widget_combobox(data_field,value=['256x256','512x512','1024x1024','2048x2048','4096x4096'], uval='cbox_reso', xsize= 100, yoffset=405, xoffset=190,ysize=25)
   case paths.res_mag of
    256:COMBOBOX_def=0
    512:COMBOBOX_def=1
    1024:COMBOBOX_def=2
    2048:COMBOBOX_def=3
    4096:COMBOBOX_def=4
  endcase
  widget_control, cbox_reso, set_combobox_select=COMBOBOX_def 

  abort= widget_button(mag_base,value='Done', uval='abort',/NO_release, xsize=110 , xoffset=740., yoffset=645, tooltip='Exit the extraction widget',ysize=25)
  exit= widget_button(mag_base,value='Exit', uval='exit',/NO_release, xsize=110 , xoffset=860., yoffset=645, tooltip='Exit CATCH',ysize=25)
  sav= widget_button(mag_base,value='Save', uval='save',/NO_release, xsize=262 , xoffset=980-512., yoffset=645, tooltip='Save the extracted parameters to the output directory',ysize=25)

  popt= widget_button(mag_base,value='Plot Options', uval='poptions',/NO_release, xsize=110 , xoffset=980-512+152., yoffset=605, tooltip='Configure plot',ysize=25) ; image scaling, grid_spacing (on/off)
  ft_calc= widget_button(mag_base,value='Flux Tube Analysis', uval='ft_calc',/NO_release, xsize=142 , xoffset=980-512., yoffset=605, tooltip='Calculate flux tube properties of the CH',ysize=25)
  
  interm_res = Widget_Base(mag_base, XOFFSET=10, yoffset=470)
  label = Widget_Label(interm_res, value='Coronal Hole Properties', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  results = Widget_Base(interm_res, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,ysize=190,xsize=428)
  label = Widget_Label(interm_res, value='Coronal Hole Properties', XOFFSET=10)

      label_smmfs = WIDGET_LABEL(results, XSIZE=250, VALUE=   'Signed Mean Magnetic Field Strength [G]   :', xoffset=10, yoffset=15-8,/ALIGN_LEFT)
      label_usmmfs = WIDGET_LABEL(results, XSIZE=250, VALUE=  'Unsigned Mean Magnetic Field Strength [G] :', xoffset=10, yoffset=30-8,/ALIGN_LEFT)
      label_skew = WIDGET_LABEL(results, XSIZE=250, VALUE=    'Skewness                                  :', xoffset=10, yoffset=45-8,/ALIGN_LEFT)
      label_smf = WIDGET_LABEL(results, XSIZE=250, VALUE=     'Signed Magnetic Flux [10^20 Mx]           :', xoffset=10, yoffset=60-8,/ALIGN_LEFT)
      label_usmf = WIDGET_LABEL(results, XSIZE=250, VALUE=    'Unsigned Magnetic Flux [10^20 Mx]         :', xoffset=10, yoffset=75-8,/ALIGN_LEFT)
      label_fb = WIDGET_LABEL(results, XSIZE=250, VALUE=      'Flux Balance [%]                          :', xoffset=10, yoffset=90-8,/ALIGN_LEFT)

      vlabel_smmfs = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=15-8)
      vlabel_usmmfs = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=30-8)
      vlabel_skew = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=45-8)
      vlabel_smf = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=60-8)
      vlabel_usmf = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=75-8)
      vlabel_fb = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=90-8)


      vlabel_smmfs_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=15-8)
      vlabel_usmmfs_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=30-8)
      vlabel_skew_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=45-8)
      vlabel_smf_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=60-8)
      vlabel_usmf_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=75-8)
      vlabel_fb_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=90-8)

      vlabel_smmfs_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=15-8)
      vlabel_usmmfs_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=30-8)
      vlabel_skew_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=45-8)
      vlabel_smf_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=60-8)
      vlabel_usmf_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=75-8)
      vlabel_fb_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=90-8)


      label_c50 = WIDGET_LABEL(results, XSIZE=250, VALUE=   'Number of Strong Flux Tubes (>50G) ', xoffset=10, yoffset=105-8,/ALIGN_LEFT)
      label_fr50 = WIDGET_LABEL(results, XSIZE=250, VALUE=  'Flux Ratio of Strong FTs (>50G) [%]', xoffset=10, yoffset=120-8,/ALIGN_LEFT)
      label_ar50 = WIDGET_LABEL(results, XSIZE=250, VALUE=  'Area Ratio of Strong FTs (>50G) [%]', xoffset=10, yoffset=135-8,/ALIGN_LEFT)
      label_clow = WIDGET_LABEL(results, XSIZE=250, VALUE=  'Number of Weak FTs (20-50G)        ', xoffset=10, yoffset=150-8,/ALIGN_LEFT)
      label_frlow = WIDGET_LABEL(results, XSIZE=250, VALUE= 'Flux Ratio of Weak FTs(20-50G) [%] ', xoffset=10, yoffset=165-8,/ALIGN_LEFT)
      label_arlow = WIDGET_LABEL(results, XSIZE=250, VALUE= 'Area Ratio of Weak FTs(20-50G) [%] ', xoffset=10, yoffset=180-8,/ALIGN_LEFT)

      vlabel_c50 = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=105-8)
      vlabel_fr50 = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=120-8)
      vlabel_ar50 = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=135-8)
      vlabel_clow = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=150-8)
      vlabel_frlow = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=165-8)
      vlabel_arlow = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=260, yoffset=180-8)


      vlabel_c50_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=105-8)
      vlabel_fr50_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=120-8)
      vlabel_ar50_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=135-8)
      vlabel_clow_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=150-8)
      vlabel_frlow_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=165-8)
      vlabel_arlow_pm = WIDGET_LABEL(results, XSIZE=8, VALUE=string(177b), xoffset=338, yoffset=180-8)

      vlabel_c50_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=105-8)
      vlabel_fr50_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=120-8)
      vlabel_ar50_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=135-8)
      vlabel_clow_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=150-8)
      vlabel_frlow_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=165-8)
      vlabel_arlow_err = WIDGET_LABEL(results, XSIZE=75, VALUE=' ', xoffset=349, yoffset=180-8)


undefine, plot_scl


  label1 = WIDGET_LABEL(mag_base, XSIZE=262, VALUE='X position:', xoffset=980-522, yoffset=522+10)
  label2 = WIDGET_LABEL(mag_base, XSIZE=262, VALUE='Y position:', xoffset=980-522, yoffset=522+25)
  label3 = WIDGET_LABEL(mag_base, XSIZE=262,  VALUE='Magnetic Field Stength [G]: ', xoffset=980-522, yoffset=522+40)

  widget_control, sav,sensitive=0
  widget_control, draww,sensitive=0
  widget_control, popt,sensitive=0
  widget_control, ft_calc,sensitive=0

  widget_control,/realize,mag_main

WIDGET_CONTROL, draww, GET_VALUE=drawID

  logo_draw_catch = widget_draw(mag_base, uvalue='logo_catch', xsize=230, ysize=75, yoffset=542+11, xoffset=740)

  WIDGET_CONTROL, logo_draw_catch, GET_VALUE=logoID_catch
  WSET, logoID_catch

  logo_path=dir+'catch.jpg'
  read_jpeg,logo_path, logo
  logo=congrid(logo,3,230*8,75*8,/center)
  tvimage, logo
  widget_control, logo_draw_catch, sensitive=0

ft_status=0

  ids={save:sav,abort:abort,exit:exit,draww:draww,label1:label1,label2:label2,label3:label3,vlabel_skew:vlabel_skew,vlabel_skew_err:vlabel_skew_err, $
        vlabel_smmfs:vlabel_smmfs,vlabel_usmmfs:vlabel_usmmfs, vlabel_smf:vlabel_smf,vlabel_usmf:vlabel_usmf,vlabel_fb:vlabel_fb,$
        vlabel_smmfs_err:vlabel_smmfs_err,vlabel_usmmfs_err:vlabel_usmmfs_err, vlabel_smf_err:vlabel_smf_err,vlabel_usmf_err:vlabel_usmf_err,$
        vlabel_fb_err:vlabel_fb_err, mag_main:mag_main,cbox_reso:cbox_reso, files_list_bin:files_list_bin, files_list_mag:files_list_mag,$
        drawID:drawID,ld_path_bin:ld_path_text_bin,sv_path:sv_path_text, ld_path_mag:ld_path_text_mag, popt:popt,thick_text:long(0),$
        vlabel_c50:vlabel_c50, vlabel_c50_err:vlabel_c50_err,vlabel_clow:vlabel_clow, vlabel_clow_err:vlabel_clow_err,$
        vlabel_fr50:vlabel_fr50, vlabel_fr50_err:vlabel_fr50_err,vlabel_frlow:vlabel_frlow, vlabel_frlow_err:vlabel_frlow_err,$
        vlabel_ar50:vlabel_ar50, vlabel_ar50_err:vlabel_ar50_err,vlabel_arlow:vlabel_arlow, vlabel_arlow_err:vlabel_arlow_err,$
        opt_main:long(0),dmin_text:long(0), grid_text:long(0),dmax_text:long(0), apply_opt:long(0),abort_opt:long(0),ft_calc:ft_calc}

  xmanager, 'mag',mag_main,  /no_block


end


PRO mag_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common mag, ids, temp_path_bin, temp_path_mag, magmap, binmap, lfiles_bin, lfiles_mag, mag_prop,plot_scl, dcord, xcord,ycord,pmap,binmaps,pindex,bindex,ft_status,binfil,reso,custom
  common version, version
  
  widget_control, ev.id, get_uvalue=uvalue                        ; get the uvalue
  
  
  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN
    
        Dx = (double(ev.x) / dcord.X_VSIZE - xcord.s[0]) / xcord.s[1]
    DY = (double(ev.Y) / dcord.y_VSIZE - Ycord.s[0]) / ycord.s[1]
    crd=[Dx,Dy]
    curr_point=[crd[0]/pindex.cdelt1+pindex.crpix1,crd[1]/pindex.cdelt2+pindex.crpix2]
    WIDGET_CONTROL, ids.label1, SET_VALUE='X position: ' + STRING(crd[0])
    WIDGET_CONTROL, ids.label2, SET_VALUE='Y position: ' + STRING(crd[1])
    border=[(0-pindex.crpix1)*pindex.cdelt1,((n_elements(binmap.data[*,0])-1)-pindex.crpix1)*pindex.cdelt1, $
      (0-pindex.crpix2)*pindex.cdelt2,((n_elements(binmap.data[0,*])-1)-pindex.crpix2)*pindex.cdelt2]
   if  crd[0] le border[0] or crd[0] ge border[1] or crd[1] le border[2] or crd[1] ge border[3] then begin
      WIDGET_CONTROL, ids.label3, SET_VALUE='Magnetic Field Stength [G]: '
    endif else begin
      WIDGET_CONTROL, ids.label3, SET_VALUE='Magnetic Field Stength [G]: ' + STRING(pmap.data[curr_point[0],curr_point[1]])
    endelse
    return
  ENDIF
    
  CASE uvalue OF
    ;##### File Management
    'draw':
    'files_mag':
    'files_bin':
    'ld_text_mag':
    'ld_text_bin':
    'sv_text':    
    'cbox_reso':
     'refresh_mag': begin
      widget_control,/hourglass
      widget_control, ids.ld_path_mag, get_value=temp_path_mag
     lfiles_mag=file_search(temp_path_mag+['*hmi*.fits','*M_96m*.fits'],/fold_case)
      lfiles_mag=file_basename(lfiles_mag)
      widget_control, ids.files_list_mag, set_value=lfiles_mag
    end
    'refresh_bin': begin
      widget_control,/hourglass
      widget_control, ids.ld_path_bin, get_value=temp_path_bin
      lfiles_bin=file_search(temp_path_bin,'*CATCH_binary_ch_mask*'+'.fits',/fold_case)
      lfiles_bin=file_basename(lfiles_bin)
      widget_control, ids.files_list_bin, set_value=lfiles_bin
    end
    'ld_path_mag': begin
      widget_control, ids.ld_path_mag, get_value=temp_path_mag
      temp_path_mag=dialog_pickfile(path=temp_path_mag,/directory)
      widget_control, ids.ld_path_mag, set_value=temp_path_mag
      lfiles_mag=file_search(temp_path_mag+['*hmi*.fits','*M_96m*.fits'],/fold_case)
      lfiles_mag=file_basename(lfiles_mag)
      widget_control, ids.files_list_mag, set_value=lfiles_mag
    end
    'ld_path_bin': begin
      widget_control, ids.ld_path_bin, get_value=temp_path_bin
      temp_path_bin=dialog_pickfile(path=temp_path_bin,/directory)
      widget_control, ids.ld_path_bin, set_value=temp_path_bin
      lfiles_bin=file_search(temp_path_bin,'*CATCH_binary_ch_mask*'+'.fits',/fold_case)
      lfiles_bin=file_basename(lfiles_bin)
      widget_control, ids.files_list_bin, set_value=lfiles_bin
    end
    'sv_path': begin
      widget_control, ids.sv_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, ids.sv_path, set_value=temp_path
    end
    'load':begin
      widget_control, /hourglass
      val=WIDGET_info(ids.files_list_mag, /list_select)
      if val eq -1 then begin & res=dialog_message( 'No magnetogram selected!', dialog_parent=ids.mag_main) & return & endif
      val2=WIDGET_info(ids.files_list_bin, /list_select)
      if val2 eq -1 then begin & res=dialog_message( 'No binary CH map selected!', dialog_parent=ids.mag_main) & return & endif
      
      widget_control, ids.ld_path_mag, set_value=temp_path_mag
      magfil=temp_path_mag+lfiles_mag[val]
      
      widget_control, ids.ld_path_bin, get_value=temp_path_bin
      binfil=file_search(temp_path_bin,lfiles_bin[val2])
      
      fits_info, magfil, /silent, n_ext=mag_ext
      if mag_ext eq 0 then magdex=headfits(magfil,errmsg=errmag,/silent) else magdex=headfits(magfil, /ex,errmsg=errmag,/silent)
      
      fits_info, binfil, /silent, n_ext=bin_ext
      if bin_ext eq 0 then bindex=headfits(binfil,errmsg=errbin,/silent) else bindex=headfits(binfil, /ex,errmsg=errbin,/silent)      
      
      ;mreadfits, magfil, magdex
      ;mreadfits, binfil, bindex

      if strmatch(sxpar(bindex, 'origin'), '*AIA*',/fold_case) ne strmatch(sxpar(magdex, 'instrume'), '*HMI*',/fold_case) or strmatch(sxpar(bindex, 'origin'), '*EIT*',/fold_case) ne strmatch(sxpar(magdex, 'instrume'), '*MDI*',/fold_case) then begin
         res=dialog_message( 'No matching input maps selected!', dialog_parent=ids.mag_main) & return 
      endif

      if strmatch(sxpar(magdex, 'instrume'), '*HMI*',/fold_case) eq 1 then magdate=sxpar(magdex, 'date-obs') else magdate=sxpar(magdex, 'date_obs')

      if anytim(magdate) lt (anytim(sxpar(bindex, 'date_obs'))-3600.*4) or anytim(magdate) gt (anytim(sxpar(bindex, 'date_obs'))+3600.*4) then begin
      res=dialog_message( 'Magnetogram time to different from binary CH map!', dialog_parent=ids.mag_main) & return
      endif
      
      plot_scl=make_array(5)
      plot_scl[3]=paths.cthick
      plot_scl[2]=paths.gridsize
      plot_scl[4]=0
      ;#magfile
      if strmatch(sxpar(magdex, 'instrume'), '*MDI*',/fold_case) eq 1 then begin      
        rd_mdi, magfil, mindex, mdata
        index2map, mindex, mdata, imagmap
        magdex=mindex
        mapid='MDI'
        plot_scl[0]=paths.mdi_range[0] & plot_scl[1]=paths.mdi_range[1]
        plot_scl[4]=1
      endif else begin
      
      if strmatch(sxpar(magdex, 'instrume'), '*HMI*',/fold_case) eq 1 then begin
        read_sdo, magfil, iheader, idata, /uncomp_del
        hmi_prep, iheader, idata, header, data;, /use_ref
        index2map, header, data, imagmap
        magdex=header
        mapid='HMI'
        plot_scl[0]=paths.hmi_range[0] & plot_scl[1]=paths.hmi_range[1] 
        plot_scl[4]=2
      endif
      endelse

        mreadfits, binfil, bindex, bindata
        index2map, bindex, bindata, binmap
        
        if TAG_EXIST(bindex,'v_catch') eq 0 then begin
          res=dialog_message( $
            ['                                    ',$
            '   CATCH binary mask was created    ',$
            '       with an old version!         ',$
            '  Compatibility is not guaranteed!  ',$
            '                                    '], dialog_parent=ids.mag_main)
        endif else begin
              
        if version ne bindex.v_catch then begin
          res=dialog_message( $
           ['                                    ',$
            '   CATCH binary mask was created    ',$
            '     with a different version!      ',$
            ' Check changelog for compatibility! ',$
            '                                    ',$
            ' Binary Mask Version: '+bindex.v_catch+' ',$
            ' Current CATCH Version: '+version+' ',$
            '                                    ',$
            '                                    '], dialog_parent=ids.mag_main)
          
        endif
      endelse
      
      reso=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      if mapid  eq 'MDI'then begin
        if reso eq '4096x4096' or reso eq '2048x2048' then begin
          res=dialog_message( ['                                   ',$
            'Resolution is greater than in file!',$
            '     Do you want to continue!      ',$
            '                                   '], dialog_parent=ids.mag_main,/question)
          if res eq 'No' then return
        endif
      endif


      case reso of
        '4096x4096':
        '2048x2048': imagmap=rebin_map(imagmap, 2048,2048)
        '1024x1024': imagmap=rebin_map(imagmap, 1024,1024)
        '512x512'  : imagmap=rebin_map(imagmap, 512,512)
        '256x256'  : imagmap=rebin_map(imagmap, 256,256)
      endcase
      
      binnum=list()
      binnum.add, where(binmap.data gt 0.5 and binmap.data lt 1.5,/null)
      binnum.add, where(binmap.data gt 0.5 and binmap.data lt 2.5,/null)
      binnum.add, where(binmap.data gt 0.5 and binmap.data lt 3.5,/null)
      binnum.add, where(binmap.data gt 0.5 and binmap.data lt 4.5,/null)
      binnum.add, where(binmap.data gt 0.5 and binmap.data lt 5.5,/null)
      
      
      if abs(n_elements(binnum[0])-n_elements(binnum[1])) le 1 and abs(n_elements(binnum[0])-n_elements(binnum[2])) le 1 and abs(n_elements(binnum[0])-n_elements(binnum[3])) le 1 and $
        abs(n_elements(binnum[0])-n_elements(binnum[4])) le 1 and abs(n_elements(binnum[1])-n_elements(binnum[2])) le 1 and abs(n_elements(binnum[1])-n_elements(binnum[3])) le 1 and $
        abs(n_elements(binnum[1])-n_elements(binnum[4])) le 1 and abs(n_elements(binnum[2])-n_elements(binnum[3])) le 1 and abs(n_elements(binnum[2])-n_elements(binnum[4])) le 1 and $
        abs(n_elements(binnum[3])-n_elements(binnum[4])) le 1 then custom=1 else custom=0
      
;      bindexes=list()
;      bindexes.add, where(binmap.data gt 0.5 and binmap.data lt 1.5)
;      bindexes.add, where(binmap.data gt 0.5 and binmap.data lt 2.5)
;      bindexes.add, where(binmap.data gt 0.5 and binmap.data lt 3.5)
;      bindexes.add, where(binmap.data gt 0.5 and binmap.data lt 4.5)
;      bindexes.add, where(binmap.data gt 0.5 and binmap.data lt 5.5)

      bmap=binmap & bmap.data[*]=0 & sbinmaps=replicate(bmap,5) 
      
      for mapi=0, 4 do sbinmaps[mapi].data[binnum[mapi]]=1
       
      
       imagmap=rem_map_limb(imagmap, missing=!values.f_nan)
       helpmap=imagmap
       imagmap=drot_map(imagmap, time=binmap.time)
       sub_map, imagmap, magmap, ref_map=binmap
       bmap=coreg_map(sbinmaps[0],magmap,/derotate,/rescale) & bmap.data[where(bmap.data gt 0,/null )]=1 & binmaps=replicate(bmap,5) 
       for mapi=1, 4 do begin
        bmap=coreg_map(sbinmaps[mapi],magmap,/derotate,/rescale)
        binmaps[mapi]=bmap
        binmaps[mapi].data[where(binmaps[mapi].data gt 0,/null)]=1
       endfor
      
       
       dex1=where(binmaps[0].data eq 1 ,/null)
       dex2=where(binmaps[1].data eq 1 ,/null)
       dex3=where(binmaps[2].data eq 1 ,/null)
       dex4=where(binmaps[3].data eq 1 ,/null)
       dex5=where(binmaps[4].data eq 1 ,/null)

      
      curve_corr, magmap,cmap, coreg=mag_corr

      magmap=helpmap
      pmap=cmap
      ;stop
      cmap.data[where(binmap.data eq 0,/null)]=!values.f_nan
      map2index, cmap, cindex

      angles=get_map_angles(cmap)
      Sonnenradius=angles.rsun
      arcsecTokm = 1/Sonnenradius*696342.
      magFlusscoreg =  mag_corr * cindex.cdelt1*cindex.cdelt2* arcsecTokm^2 *1e10; 1e10!!!!  ;correction of the magnetic flux because of the radial magnetic field
      flux_px=magFlusscoreg*cmap.data

      ;      helpvar=w_mean(meanint_prop[0:4,0],meanint_prop[0:4,2],meanint_prop[0:4,1])
;      ch_prop[5,i]=helpvar[0] & ch_prop[6,i]=helpvar[1]


      mag_prop=make_array(7,13) & mag_prop[*]=!values.f_nan
      mag_prop[0,0]=total(flux_px[dex1],/NaN)
      mag_prop[1,0]=total(flux_px[dex2],/NaN)
      mag_prop[2,0]=total(flux_px[dex3],/NaN)
      mag_prop[3,0]=total(flux_px[dex4],/NaN)
      mag_prop[4,0]=total(flux_px[dex5],/NaN)
      helpvar=moment(mag_prop[0:4,0]/1e20,/nan, maxmoment=1)
      mag_prop[5,0]=helpvar[0]*1e20
      mag_prop[6,0]=max(sqrt(mag_prop[0:4,0]-mag_prop[5,0])^2)
      
      mag_prop[0,1]=total(abs(flux_px[dex1]),/NaN)
      mag_prop[1,1]=total(abs(flux_px[dex2]),/NaN)
      mag_prop[2,1]=total(abs(flux_px[dex3]),/NaN)
      mag_prop[3,1]=total(abs(flux_px[dex4]),/NaN)
      mag_prop[4,1]=total(abs(flux_px[dex5]),/NaN)
      helpvar=moment(mag_prop[0:4,1]/1e20,/nan, maxmoment=1)
      mag_prop[5,1]=helpvar[0]*1e20
      mag_prop[6,1]=max(sqrt(mag_prop[0:4,1]-mag_prop[5,1])^2)
      
      mom=moment((cmap.data[dex1]),/nan,maxmoment=1) & mag_prop[0,2]=mom[0]
      mom=moment((cmap.data[dex2]),/nan,maxmoment=1) & mag_prop[1,2]=mom[0]
      mom=moment((cmap.data[dex3]),/nan,maxmoment=1) & mag_prop[2,2]=mom[0]
      mom=moment((cmap.data[dex4]),/nan,maxmoment=1) & mag_prop[3,2]=mom[0]
      mom=moment((cmap.data[dex5]),/nan,maxmoment=1) & mag_prop[4,2]=mom[0]
      helpvar=moment(mag_prop[0:4,2],/nan, maxmoment=1) & mag_prop[5,2]=helpvar[0]
      mag_prop[6,2]=max(sqrt(mag_prop[0:4,2]-mag_prop[5,2])^2)
      
      mom=moment((abs(cmap.data[dex1])),/nan,maxmoment=1) & mag_prop[0,3]=mom[0]
      mom=moment((abs(cmap.data[dex2])),/nan,maxmoment=1) & mag_prop[1,3]=mom[0]
      mom=moment((abs(cmap.data[dex3])),/nan,maxmoment=1) & mag_prop[2,3]=mom[0]
      mom=moment((abs(cmap.data[dex4])),/nan,maxmoment=1) & mag_prop[3,3]=mom[0]
      mom=moment((abs(cmap.data[dex5])),/nan,maxmoment=1) & mag_prop[4,3]=mom[0]
      helpvar=moment(mag_prop[0:4,3],/nan, maxmoment=1) & mag_prop[5,3]=helpvar[0]
      mag_prop[6,3]=max(sqrt(mag_prop[0:4,3]-mag_prop[5,3])^2)

      mag_prop[0,4]=abs(mag_prop[0,0]/mag_prop[0,1])*100.
      mag_prop[1,4]=abs(mag_prop[1,0]/mag_prop[1,1])*100.
      mag_prop[2,4]=abs(mag_prop[2,0]/mag_prop[2,1])*100.
      mag_prop[3,4]=abs(mag_prop[3,0]/mag_prop[3,1])*100.
      mag_prop[4,4]=abs(mag_prop[4,0]/mag_prop[4,1])*100.
      helpvar=moment(mag_prop[0:4,4],/nan, maxmoment=1)
      mag_prop[5,4]=helpvar[0]
      mag_prop[6,4]=max(sqrt(mag_prop[0:4,4]-mag_prop[5,4])^2)

      mag_prop[0,5]=(moment(cmap.data[dex1],/nan,maxmoment=3))[2]
      mag_prop[1,5]=(moment(cmap.data[dex2],/nan,maxmoment=3))[2]
      mag_prop[2,5]=(moment(cmap.data[dex3],/nan,maxmoment=3))[2]
      mag_prop[3,5]=(moment(cmap.data[dex4],/nan,maxmoment=3))[2]
      mag_prop[4,5]=(moment(cmap.data[dex5],/nan,maxmoment=3))[2]
      helpvar=moment(mag_prop[0:4,5],/nan, maxmoment=2)
      mag_prop[5,5]=helpvar[0]
      mag_prop[6,5]=sqrt(helpvar[1])
      
      
      mag_prop[0,12]=bindex.area_1
      mag_prop[1,12]=bindex.area_2
      mag_prop[2,12]=bindex.area_3
      mag_prop[3,12]=bindex.area_4
      mag_prop[4,12]=bindex.area_5
      helpvar=moment(mag_prop[0:4,12]/1e10,/nan, maxmoment=1)
      mag_prop[5,12]=helpvar[0]*1e10
      mag_prop[6,12]=max(sqrt((mag_prop[0:4,12]-mag_prop[5,12])^2))

      if custom eq 1 then begin
        for k=0, 5 do mag_prop[6,k]=!values.f_nan
        mag_prop[6,12]=!values.f_nan
      endif
      
      WIDGET_CONTROL, ids.vlabel_smmfs, SET_VALUE=strtrim(STRING(mag_prop[5,2], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_usmmfs, SET_VALUE=strtrim(STRING(mag_prop[5,3], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_smf, SET_VALUE=strtrim(STRING(mag_prop[5,0]/1e20, format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_usmf, SET_VALUE=strtrim(STRING(mag_prop[5,1]/1e20, format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_fb, SET_VALUE=strtrim(STRING(mag_prop[5,4], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_skew, SET_VALUE=strtrim(STRING(mag_prop[5,5], format='(F15.3)'),2)


      WIDGET_CONTROL, ids.vlabel_smmfs_err, SET_VALUE=strtrim(STRING(mag_prop[6,2], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_usmmfs_err, SET_VALUE=strtrim(STRING(mag_prop[6,3], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_smf_err, SET_VALUE=strtrim(STRING(mag_prop[6,0]/1e20, format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_usmf_err, SET_VALUE=strtrim(STRING(mag_prop[6,1]/1e20, format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_fb_err, SET_VALUE=strtrim(STRING(mag_prop[6,4], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_skew_err, SET_VALUE=strtrim(STRING(mag_prop[6,5], format='(F15.3)'),2)
      
      WIDGET_CONTROL, ids.vlabel_c50, SET_VALUE=strtrim(STRING((mag_prop[5,6]), format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_fr50, SET_VALUE=strtrim(STRING(mag_prop[5,7], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_ar50, SET_VALUE=strtrim(STRING(mag_prop[5,8], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_clow, SET_VALUE=strtrim(STRING((mag_prop[5,9]), format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_frlow, SET_VALUE=strtrim(STRING(mag_prop[5,10], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_arlow, SET_VALUE=strtrim(STRING(mag_prop[5,11], format='(F15.3)'),2)

      WIDGET_CONTROL, ids.vlabel_c50_err, SET_VALUE=strtrim(STRING((mag_prop[6,6]), format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_fr50_err, SET_VALUE=strtrim(STRING(mag_prop[6,7], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_ar50_err, SET_VALUE=strtrim(STRING(mag_prop[6,8], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_clow_err, SET_VALUE=strtrim(STRING((mag_prop[6,9]), format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_frlow_err, SET_VALUE=strtrim(STRING(mag_prop[6,10], format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_arlow_err, SET_VALUE=strtrim(STRING(mag_prop[6,11], format='(F15.3)'),2)
      
      WSET, ids.drawID
      loadct,0,/silent
      
    
      map2index, binmap,bindex
      map2index, pmap, pindex
      sz=size(pmap.data)
      if sz[1] ge sz[2] then begin
        yz=sz[2]*(440./sz[1]) & yoffs=((440.-yz)/2.)/440.
        pos=[0.12,0.12+yoffs,0.98,0.98-yoffs]
      endif
      if sz[2] gt sz[1] then begin
        xz=sz[1]*(512./sz[2]) & xoffs=((512.-xz)/2.)/512.
        pos=[0.12+xoffs,0.12,0.98-xoffs,0.98]
      endif


      
      plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1],window=!D.Window, position=pos, grid_spacing=plot_scl[2], tit=' '
      
      if custom eq 0 then begin
      emap=binmaps[4] & emap.data[where(binmaps[0].data eq 1,/null)]=0
      plot_map, emap, /over, /cont, color=cgcolor('blue'),/overlay, contour=1,/cell_fill,levels=[1]
      endif
      
      plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0
      
      xyouts, 0.5,0.01,strmid(bindex.date_obs,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5,/norm, alignment=0.5
      dcord=!D & xcord=!X & ycord=!Y
      
      ft_status=0
      widget_control, ids.draww,sensitive=1
      widget_control, ids.save,sensitive=1
      widget_control, ids.popt,sensitive=1
      widget_control, ids.ft_calc,sensitive=1

  end
  'poptions': begin
    widget_control, ids.mag_main, TLB_GET_OFFSET=offs
    
    opt_main = widget_base(title='Options', xsize=170, ysize=220,SCR_XSIZE=170,SCR_YSIZE=220,modal=(1-debug), group_leader=ids.mag_main, xoffset=offs[0]+634, yoffset=offs[1]+171)

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
    grid_text=widget_TEXT(opt,value=strtrim(string(plot_scl[2],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=90, ysize=1, uvaL='grid_text', units=0,/editable,/ALL_EVENTS)
    label_thick = WIDGET_LABEL(opt, XSIZE=50, VALUE='Thick:', xoffset=10, yoffset=135, /align_left)
    thick_text=widget_TEXT(opt,value=strtrim(string(plot_scl[3],format='(I)'),2), xsize=7, XOFFSET=70,yoffset=135, ysize=1, uvaL='thick_text', units=0,/editable,/ALL_EVENTS)

    abort_opt= widget_button(opt,value='Abort', uval='abort_opt',/NO_release, xsize=60 , xoffset=10., yoffset=170, tooltip='Close options window',ysize=25)
    apply_opt= widget_button(opt,value='Apply', uval='ap_opt',/NO_release, xsize=60 , xoffset=80., yoffset=170, tooltip='Apply options',ysize=25)

    ids.opt_main=opt_main & ids.dmin_text=dmin_text & ids.grid_text=grid_text
    ids.dmax_text=dmax_text & ids.apply_opt=apply_opt & ids.abort_opt=abort_opt & ids.thick_text=thick_text


    widget_control, opt_main, /realize

    xmanager, 'optmg',opt_main,  /no_block
  end
  'ft_calc': begin
     widget_control,/hourglass
     widget_control, ids.mag_main, TLB_GET_OFFSET=offs
     loadingbar_main = widget_base(title='Calculating....', xsize=270, ysize=50,SCR_XSIZE=270,SCR_YSIZE=50,modal=(1-debug), group_leader=ids.mag_main, xoffset=offs[0]+584, yoffset=offs[1]+256)

     loadingbar = widget_draw(loadingbar_main, uvalue='loadingbar', xsize=250, ysize=30, xoffset=10, yoffset=10, RENDERER=1)


     widget_control,/realize,loadingbar
     widget_control,loadingbar, sensitive=0
     WIDGET_CONTROL, loadingbar, GET_VALUE=logoID_loadingbar
     WSET, logoID_loadingbar
     bar=make_array(3,250,30)
     tvimage, bar
    ;####################
    
    barcount=0
    
    if custom eq 1 then begin
      repft=0 
      modsize=250.
    endif else begin
      repft=4
      modsize=50.
    endelse

  
    for count_bmap=0, repft do begin
    
    
    
    help_mag_map=pmap;magmap; & help_mag_map.data[*]
    ;pmap
    help_mag_map.data[where(binmaps[count_bmap].data ne 1,/null)]= !values.f_nan; magmap.data[where(binmaps[count_bmap].data eq 1)]
    ;stop
    
    extract_ch, help_mag_map, out, -20, 20,/inverse,/silent   ; extract FT +/- 20 G

    modular=(modsize/n_elements(out))
    ;print, modular
    angles=get_map_angles(pmap)
    Sonnenradius=angles.rsun
    arcsecTokm = 1/Sonnenradius*696342
    
    ft_w=list()
    ft_area_w=list()
    ft_flux_w=list()
    ft_s=list()
    ft_area_s=list()
    ft_flux_s=list()

    for j=0, n_elements(out)-1 do begin
      ;-------------------------
      ;if j mod modular eq 0 then begin
        if barcount lt (249-modular) then begin
        barcount=barcount+modular
        bar[2,0:barcount,*]=255 & bar[1,0:barcount,*]=255
        tvimage, bar
        endif
      ;endif
      ;-------------------------
      curve_corr, out[j], ftcomap, area=ftar, coreg=ft_corr
      map2index, out[j], oindex

      ft_magFlusscoreg =  ft_corr * oindex.cdelt1*oindex.cdelt2* arcsecTokm^2 *1e10  ;correction of the magnetic flux because of the radial magnetic field
      ft_flux_px=ft_magFlusscoreg*out[j].data

      open_ft=total(ft_flux_px,/NaN)
      total_ft=total(abs(ft_flux_px),/NaN)

      ft_mag=moment(out[j].data,/nan,maxmoment=0)
      ft_mag=ft_mag[0]

      if abs(ft_mag) gt 20 and abs(ft_mag) lt 50 then  begin
        ft_w.add, out[j]
        ft_area_w.add, ftar
        ft_flux_w.add, open_ft
      endif
      if abs(ft_mag) gt 50 then begin
        ft_s.add, out[j]
        ft_area_s.add, ftar
        ft_flux_s.add, open_ft
      endif

    endfor
    
    mag_prop[count_bmap,6]=n_elements(ft_s)
    if mag_prop[count_bmap,6] eq 0 then begin
          mag_prop[count_bmap,7]=!values.f_nan
          mag_prop[count_bmap,8]=!values.f_nan
      endif else begin
        ft_area_s=ft_area_s.toarray()
        ft_flux_s=ft_flux_s.toarray()
        mag_prop[count_bmap,7]=total(ft_flux_s,/nan)/mag_prop[count_bmap,0]*100.
        mag_prop[count_bmap,8]=total(ft_area_s,/nan)/mag_prop[count_bmap,12]*100.
      endelse

    mag_prop[count_bmap,9]=n_elements(ft_w)
    if mag_prop[count_bmap,9] eq 0 then begin
          mag_prop[count_bmap,10]=!values.f_nan
          mag_prop[count_bmap,11]=!values.f_nan
      endif else begin
          ft_area_w=ft_area_w.toarray()
          ft_flux_w=ft_flux_w.toarray()
          mag_prop[count_bmap,10]=total(ft_flux_w,/nan)/mag_prop[count_bmap,0]*100.
          mag_prop[count_bmap,11]=total(ft_area_w,/nan)/mag_prop[count_bmap,12]*100.
      endelse
    
   endfor 
   

    
    if custom eq 1 then begin
      for k=6, 11 do begin
        mag_prop[5,k]=mag_prop[0,k]
        mag_prop[6,k]=!values.f_nan
      endfor     
      WIDGET_CONTROL, ids.vlabel_c50_err, SET_VALUE=strtrim(STRING((mag_prop[6,6]), format='(F15.3)'),2)
      WIDGET_CONTROL, ids.vlabel_clow_err, SET_VALUE=strtrim(STRING((mag_prop[6,9]), format='(F15.3)'),2)
    endif else begin
    helpvar=moment(mag_prop[0:4,6],/nan, maxmoment=1)
    mag_prop[5,6]=helpvar[0]
    mag_prop[6,6]=max(sqrt(mag_prop[0:4,6]-mag_prop[5,6])^2)
    
    helpvar=moment(mag_prop[0:4,7],/nan, maxmoment=1)
    mag_prop[5,7]=helpvar[0]
    mag_prop[6,7]=max(sqrt(mag_prop[0:4,7]-mag_prop[5,7])^2)
    
    helpvar=moment(mag_prop[0:4,8],/nan, maxmoment=1)
    mag_prop[5,8]=helpvar[0]
    mag_prop[6,8]=max(sqrt(mag_prop[0:4,8]-mag_prop[5,8])^2)
    
    helpvar=moment(mag_prop[0:4,9],/nan, maxmoment=1)
    mag_prop[5,9]=helpvar[0]
    mag_prop[6,9]=max(sqrt(mag_prop[0:4,9]-mag_prop[5,9])^2)
    
    helpvar=moment(mag_prop[0:4,10],/nan, maxmoment=1)
    mag_prop[5,10]=helpvar[0]
    mag_prop[6,10]=max(sqrt(mag_prop[0:4,10]-mag_prop[5,10])^2)
    
    helpvar=moment(mag_prop[0:4,11],/nan, maxmoment=1)
    mag_prop[5,11]=helpvar[0]
    mag_prop[6,11]=max(sqrt(mag_prop[0:4,11]-mag_prop[5,11])^2)
    
    
    WIDGET_CONTROL, ids.vlabel_c50_err, SET_VALUE=strtrim(STRING(round(mag_prop[6,6]), format='(I)'),2)
    WIDGET_CONTROL, ids.vlabel_clow_err, SET_VALUE=strtrim(STRING(round(mag_prop[6,9]), format='(I)'),2)
    endelse
    
    WIDGET_CONTROL, ids.vlabel_c50, SET_VALUE=strtrim(STRING(round(mag_prop[5,6]), format='(I)'),2)
    WIDGET_CONTROL, ids.vlabel_fr50, SET_VALUE=strtrim(STRING(mag_prop[5,7], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_ar50, SET_VALUE=strtrim(STRING(mag_prop[5,8], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_clow, SET_VALUE=strtrim(STRING(round(mag_prop[5,9]), format='(I)'),2)
    WIDGET_CONTROL, ids.vlabel_frlow, SET_VALUE=strtrim(STRING(mag_prop[5,10], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_arlow, SET_VALUE=strtrim(STRING(mag_prop[5,11], format='(F15.3)'),2)

    WIDGET_CONTROL, ids.vlabel_fr50_err, SET_VALUE=strtrim(STRING(mag_prop[6,7], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_ar50_err, SET_VALUE=strtrim(STRING(mag_prop[6,8], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_frlow_err, SET_VALUE=strtrim(STRING(mag_prop[6,10], format='(F15.3)'),2)
    WIDGET_CONTROL, ids.vlabel_arlow_err, SET_VALUE=strtrim(STRING(mag_prop[6,11], format='(F15.3)'),2)
    
    
    
    
    
    ft_status=1
    WSET, ids.drawID
    WIDGET_CONTROL, loadingbar_main,/destroy
  
    end  
  ;#### Finish
  'save': begin
    
      WIDGET_CONTROL, /HOURGLASS
      widget_control, ids.sv_path, get_value=temp_path
      if file_test(temp_path,/directory) ne 1 then begin
          res=dialog_message('No output directory specified!', dialog_parent=ids.ex_main)
          return
      endif

      if strmatch(pindex.origin, '*MDI*',/fold_case) eq 1 then obs='MDI' else obs='HMI'
      
          helpdate=anytim(bindex.date_obs,/ex)
          yyyy=strtrim(string(helpdate[6]),2)
          if helpdate[5] lt 10 then mm='0'+strtrim(string(helpdate[5]),2) else mm=strtrim(string(helpdate[5]),2)
          if helpdate[4] lt 10 then dd='0'+strtrim(string(helpdate[4]),2) else dd=strtrim(string(helpdate[4]),2)
          if helpdate[0] lt 10 then hh='0'+strtrim(string(helpdate[0]),2) else hh=strtrim(string(helpdate[0]),2)
          if helpdate[1] lt 10 then mimi='0'+strtrim(string(helpdate[1]),2) else mimi=strtrim(string(helpdate[1]),2)
          if helpdate[2] lt 10 then ss='0'+strtrim(string(helpdate[2]),2) else ss=strtrim(string(helpdate[2]),2)
          date_format=yyyy+mm+dd+'_'+hh+mimi+ss
          
          dir_name=date_format+'_CATCH/'
          if FILE_TEST(temp_path+dir_name, /DIRECTORY) eq 0 then file_mkdir, temp_path+dir_name
          save_path=temp_path+dir_name
          
          binname=file_basename(binfil)
          strg=stregex(binname,'mask_.*fits$',/ex,/SUBEXPR)
          reso_binv=stregex(binname,'x.*_',/ex,/SUBEXPR)
          case strmid(reso_binv,0,5) of
            'x4096': reso_bin='4096x4096'
            'x2048': reso_bin='2048x2048'
            'x1024': reso_bin='1024x1024'
            'x512_':  reso_bin='512x512'
            'x256_':  reso_bin='256x256'
          endcase
          
        if strg eq '' then begin            
        outname_prop=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_ch_mag_properties.txt'
        outname_save=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_mag_maps.sav'
        outname_fd=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_FD.eps'
        outname_mag=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap.eps'
        outname_fdpng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_FD.png'
        outname_magpng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap.png'
        outname_fts=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_FT_map.eps
        outname_ftspng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_FT_map.png
      endif else begin
        strg_len=strlen(strg)
        case strg_len of
          11: file_number=fix(strmid(strg, 5,1))
          12: file_number=fix(strmid(strg, 5,2))
          13: file_number=fix(strmid(strg, 5,3))
          14: file_number=fix(strmid(strg, 5,4))
        endcase
        outname_prop=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_ch_mag_properties_'+strtrim(string(file_number),2)+'.txt'
        outname_save=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_mag_maps_'+strtrim(string(file_number),2)+'.sav'
        outname_fd=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_FD_'+strtrim(string(file_number),2)+'.eps'
        outname_mag=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_'+strtrim(string(file_number),2)+'.eps'
        outname_fdpng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_FD_'+strtrim(string(file_number),2)+'.png'
        outname_magpng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_magmap_'+strtrim(string(file_number),2)+'.png'
        outname_fts=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_FT_map_'+strtrim(string(file_number),2)+'.eps
        outname_ftspng=save_path+date_format+'_'+obs+'_'+reso_bin+'_'+reso+'_FT_map_'+strtrim(string(file_number),2)+'.png 
      endelse
      

      magproperties2txt, mag_prop,outname_prop , obs, strmid(pindex.date_obs,0,19)
      
      if paths.smaps_mag eq 'y' then save, filename=outname_save, description='CATCH:MAG', pmap,binmap
      
      if paths.mag_eps eq 'y' then begin
        

      x_size=10       ;x size in cm
      y_size=10     ;x size in cm
      x_offset=0
      y_offset=0
      
      loadct,0,/silent

      SET_PLOT, 'PS'
      device, filename=outname_fd,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0,BITS_PER_PIXEL=24

      plot_map, magmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
        position=[0.2,0.2,0.9,0.9],grid_spacing=plot_scl[2],charthick=2,charsize=0.7, xrange=[-1100,1100], yrange=[-1100,1100] 
      plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0
      
      logo_path=dir+'catch.jpg'
      read_jpeg,logo_path, logo
      ;logo=congrid(logo,3,230*8,75*8,/center)
      tvimage, logo, position=[0.746,0.2,0.9,0.25]
      
      DEVICE, /CLOSE_FILE
      SET_PLOT, 'X'
      
      loadct,0,/silent
      
      SET_PLOT, 'PS'
      device, filename=outname_mag,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0,BITS_PER_PIXEL=24

      plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
        grid_spacing=plot_scl[2],charthick=2,charsize=0.7
      plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0

      DEVICE, /CLOSE_FILE
      SET_PLOT, 'X'
      
      endif
      
      if paths.ft_eps eq 'y' and ft_status eq 1 then begin


        x_size=10       ;x size in cm
        y_size=10     ;x size in cm
        x_offset=0
        y_offset=0

        loadct,0,/silent


        SET_PLOT, 'PS'
        device, filename=outname_fts,xsize = x_size, ysize = y_size, xoffset = x_offset, yoffset = y_offset, encaps = 1,color=1,decomposed=1, landscape=0,BITS_PER_PIXEL=24

  emap=pmap & emap.data[*]=!values.f_nan & emap.data[where(binmaps[2].data eq 1,/null)]=pmap.data[where(binmaps[2].data eq 1,/null)]
  extract_ch, emap, out, -20, 20,/inverse,/silent   ; extract FT +/- 20 G

  restore, dir+'ft_colors.sav'
  TVLCT, r, g, b
  
      plot_map, pmap,dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
        grid_spacing=plot_scl[2],charthick=2,charsize=0.7
        

;  xax =[-1,-1,1,1,-1] & yax=[-1,1,1,-1,-1] & USERSYM, xax, yax, /fill

;imgsize=(pmap.xc+n_elements(pmap.data[*,0])/2*pmap.dx)-(pmap.xc-n_elements(pmap.data[*,0])/2*pmap.dx)
;ssz=imgsize*0.00002
;ssz=pmap.dx*0.000015

plotsym,8,1./(n_elements(pmap.data[*,0]))^3*1e3,/fill
 
; print, 1./(n_elements(pmap.data[*,0]))^3*1e3
 
  for ftc=0, 3 do begin
    
  
  for i=0, n_elements(out)-1 do begin

    curve_corr, out[i], cout , mag1=mag1; 1/cos corr
    meanm=abs(mag1);mean(out[i].data,/nan)
    outi=cout


    ftm=outi
    ftm.data[*]=0
    ftm.data[where(finite(outi.data) eq 1,/null)]=1
    dim_pixel_arr=where(ftm.data eq 1.0,/null)
    n_dim=n_elements(dim_pixel_arr)
    xp=get_map_prop(ftm,/xp)
    yp=get_map_prop(ftm,/yp)

    if (n_dim ne 0) then begin
      x0=make_array(1,n_dim, /float, value=0.0)
      y0=make_array(1,n_dim, /float, value=0.0)
      for j=0, n_dim-1 do begin
        x0[j]=xp[[dim_pixel_arr[j]]]
        y0[j]=yp[[dim_pixel_arr[j]]]
      endfor
      
      case ftc of
      0: begin & if meanm ge 20 and meanm lt 50 then begin & pcol=cgcolor('black') & plots, x0,y0, psym=8, color=pcol,symsize=1.6 & endif & end
      1: begin & if meanm ge 20 and meanm lt 50 then begin & pcol=cgcolor('magenta') & plots, x0,y0, psym=8, color=pcol,symsize=1.2 & endif & end
      2: begin & if meanm ge 50 then begin & pcol=cgcolor('black') & plots, x0,y0, psym=8, color=pcol,symsize=1.6 & endif & end
      3: begin & if meanm ge 50 then begin & pcol=cgcolor('cyan') & plots, x0,y0, psym=8, color=pcol,symsize=1.2 & endif & end
      endcase
      
      
    endif

  endfor
  endfor

  plmap=binmaps[2]
  ;plmap.data=median(plmap.data,8)
  plot_map, plmap, /over, /cont,c_color=cgcolor('white'), c_thick=plot_scl[3]+1 & plot_map, plmap, /over,/cont, c_color=cgcolor('black'), c_thick=plot_scl[3]



        DEVICE, /CLOSE_FILE
        SET_PLOT, 'X'

      endif
;##########
if paths.mag_png eq 'y' then begin

  Set_Plot, 'Z'
  Device, Decomposed=1, Set_Pixel_Depth=24, Set_Resolution=[2160,2160];,decomposed=1;,BITS_PER_PIXEL=24;,color=1
  !p.multi=0

  !P.Color = '000000'xL
  !P.Background = 'FFFFFF'xL

  loadct,0, /silent
  
  plot_map, magmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
    position=[0.2,0.2,0.9,0.9],grid_spacing=plot_scl[2],charthick=6,charsize=4, xrange=[-1100,1100], yrange=[-1100,1100]
  plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0

  write_png, outname_fdpng,  tvrd(/true)
  
  loadct,0, /silent
  
  plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
    grid_spacing=plot_scl[2],charthick=6,charsize=4
  plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0
  
  write_png, outname_magpng,  tvrd(/true)
  
Set_Plot, 'X'
endif

if paths.ft_png eq 'y' then begin

  Set_Plot, 'Z'
  Device, Decomposed=1, Set_Pixel_Depth=24, Set_Resolution=[2160,2160];,decomposed=1;,BITS_PER_PIXEL=24;,color=1
  !p.multi=0

  !P.Color = '000000'xL
  !P.Background = 'FFFFFF'xL


  emap=pmap & emap.data[*]=!values.f_nan & emap.data[where(binmaps[2].data eq 1,/null)]=pmap.data[where(binmaps[2].data eq 1,/null)]
  extract_ch, emap, out, -20, 20,/inverse,/silent   ; extract FT +/- 20 G

  restore, dir+'ft_colors.sav'
  TVLCT, r, g, b

  plot_map, pmap,dmin=plot_scl[0],dmax=plot_scl[1], title=strmid(pindex.date_obs,0,16),$
    grid_spacing=plot_scl[2],charthick=6,charsize=4

plotsym,8,1./(n_elements(pmap.data[*,0]))^3*1e3,/fill

 
  for ftc=0, 3 do begin
    
  
  for i=0, n_elements(out)-1 do begin

    curve_corr, out[i], cout , mag1=mag1; 1/cos corr
    meanm=abs(mag1);mean(out[i].data,/nan)
    outi=cout


    ftm=outi
    ftm.data[*]=0
    ftm.data[where(finite(outi.data) eq 1,/null)]=1
    dim_pixel_arr=where(ftm.data eq 1.0,/null)
    n_dim=n_elements(dim_pixel_arr)
    xp=get_map_prop(ftm,/xp)
    yp=get_map_prop(ftm,/yp)

    if (n_dim ne 0) then begin
      x0=make_array(1,n_dim, /float, value=0.0)
      y0=make_array(1,n_dim, /float, value=0.0)
      for j=0, n_dim-1 do begin
        x0[j]=xp[[dim_pixel_arr[j]]]
        y0[j]=yp[[dim_pixel_arr[j]]]
      endfor
      
      case ftc of
      0: begin & if meanm ge 20 and meanm lt 50 then begin & pcol=cgcolor('black') & plots, x0,y0, psym=8, color=pcol,symsize=15 & endif & end
      1: begin & if meanm ge 20 and meanm lt 50 then begin & pcol=cgcolor('magenta') & plots, x0,y0, psym=8, color=pcol,symsize=10 & endif & end
      2: begin & if meanm ge 50 then begin & pcol=cgcolor('black') & plots, x0,y0, psym=8, color=pcol,symsize=15 & endif & end
      3: begin & if meanm ge 50 then begin & pcol=cgcolor('cyan') & plots, x0,y0, psym=8, color=pcol,symsize=10 & endif & end
      endcase
      
      
    endif

  endfor
  endfor

  plmap=binmaps[2]
  plot_map, plmap, /over, /cont,c_color=cgcolor('white'), c_thick=plot_scl[3]+2 & plot_map, plmap, /over,/cont, c_color=cgcolor('black'), c_thick=plot_scl[3]+1

 write_png, outname_ftspng,  tvrd(/true)

  Set_Plot, 'X'
endif
      
        res=dialog_message( ['                           ',$
                             ' Files sucessfully saved!  ',$
                             '                           ',$
                             ' Properties (Mag) [y]      ',$
                             ' Maps save file   ['+strtrim(string(paths.smaps_mag),2)+']      ',$
                             ' .eps Image (Mag) ['+strtrim(string(paths.mag_eps),2)+']      ',$
                             ' .eps Image (FTs) ['+strtrim(string(paths.ft_eps),2)+']      ',$
                             ' .png Image (Mag) ['+strtrim(string(paths.mag_png),2)+']      ',$
                             ' .png Image (FTs) ['+strtrim(string(paths.ft_png),2)+']      ',$
                             '                           '], dialog_parent=ids.mag_main,/information)
        
  end
  'abort':  begin
    
    IF paths.lock eq 'off' then begin
      new_paths=paths
      changes=0
      widget_control, ids.ld_path_mag, get_value=temp_path
      if temp_path ne paths.magpath then begin & changes+=1 & new_paths.magpath=temp_path & endif
      widget_control, ids.ld_path_bin, get_value=temp_path
      if temp_path ne paths.outpath then begin & changes+=1 & new_paths.outpath=temp_path & endif

      resol=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      case resol of
        '4096x4096': res_val=4096
        '2048x2048': res_val=2048
        '1024x1024': res_val=1024
        '512x512'  : res_val=512
        '256x256'  : res_val=256
      endcase
      if res_val ne paths.res_mag then begin & changes+=1 & new_paths.res_mag=res_val & endif

      if isa(plot_scl) eq 1 then begin
        if plot_scl[2] ne paths.gridsize then begin & changes+=1 & new_paths.gridsize=plot_scl[3] & endif
        if plot_scl[3] ne paths.cthick then begin & changes+=1 & new_paths.cthick=plot_scl[2] & endif
        case plot_scl[4] of
          1: begin
            if plot_scl[0] ne paths.mdi_range[0] then begin & changes+=1 & new_paths.mdi_range[0]=plot_scl[0] & endif
            if plot_scl[1] ne paths.mdi_range[1] then begin & changes+=1 & new_paths.mdi_range[1]=plot_scl[1] & endif
          end
          2: begin
            if plot_scl[0] ne paths.hmi_range[0] then begin & changes+=1 & new_paths.hmi_range[0]=plot_scl[0] & endif
            if plot_scl[1] ne paths.hmi_range[1] then begin & changes+=1 & new_paths.hmi_range[1]=plot_scl[1] & endif
          end
        endcase
      endif

      if changes gt 0 then begin
        write_ini_catch, dir+'/config_CATCH.ini', new_paths,ids.mag_main, /struct_old
        if  systim(/sec)-file_modtime(dir+'/config_CATCH.ini') lt 10 then  paths=new_paths
      endif

    ENDIF
    WIDGET_CONTROL, ids.mag_main,/destroy 
    end
  'exit': begin
    IF paths.lock eq 'off' then begin
      new_paths=paths
      changes=0
      widget_control, ids.ld_path_mag, get_value=temp_path
      if temp_path ne paths.magpath then begin & changes+=1 & new_paths.magpath=temp_path & endif
      widget_control, ids.ld_path_bin, get_value=temp_path
      if temp_path ne paths.outpath then begin & changes+=1 & new_paths.outpath=temp_path & endif

      resol=WIDGET_info(ids.cbox_reso, /combobox_gettext)
      case resol of
        '4096x4096': res_val=4096
        '2048x2048': res_val=2048
        '1024x1024': res_val=1024
        '512x512'  : res_val=512
        '256x256'  : res_val=256
      endcase
      if res_val ne paths.res_mag then begin & changes+=1 & new_paths.res_mag=res_val & endif

      if isa(plot_scl) eq 1 then begin
        if plot_scl[2] ne paths.gridsize then begin & changes+=1 & new_paths.gridsize=plot_scl[3] & endif
        if plot_scl[3] ne paths.cthick then begin & changes+=1 & new_paths.cthick=plot_scl[2] & endif
        case plot_scl[4] of
          1: begin
            if plot_scl[0] ne paths.mdi_range[0] then begin & changes+=1 & new_paths.mdi_range[0]=plot_scl[0] & endif
            if plot_scl[1] ne paths.mdi_range[1] then begin & changes+=1 & new_paths.mdi_range[1]=plot_scl[1] & endif
          end
          2: begin
            if plot_scl[0] ne paths.hmi_range[0] then begin & changes+=1 & new_paths.hmi_range[0]=plot_scl[0] & endif
            if plot_scl[1] ne paths.hmi_range[1] then begin & changes+=1 & new_paths.hmi_range[1]=plot_scl[1] & endif
          end
        endcase
      endif

      if changes gt 0 then begin
        write_ini_catch, dir+'/config_CATCH.ini', new_paths,ids.mag_main, /struct_old
        if  systim(/sec)-file_modtime(dir+'/config_CATCH.ini') lt 10 then  paths=new_paths
      endif

    ENDIF
    
     WIDGET_CONTROL, ids.mag_main,/destroy & WIDGET_CONTROL, id.main,/destroy & end
  else: dummy=0
ENDCASE 

END

PRO optmg_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common mag, ids, temp_path_bin, temp_path_mag, magmap, binmap, lfiles_bin, lfiles_mag, mag_prop,plot_scl, dcord, xcord,ycord,pmap,binmaps,pindex,bindex,ft_status,binfil,reso,custom

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
        widget_control, ids.thick_text, get_value=nthick
        plot_scl[0]=fix(ndmin) &  plot_scl[1]=fix(ndmax) & plot_scl[2]=fix(ngrid) & plot_scl[3]=fix(nthick)

      WSET, ids.drawID

      loadct,0,/silent
      
      sz=size(pmap.data)
      if sz[1] ge sz[2] then begin
        yz=sz[2]*(440./sz[1]) & yoffs=((440.-yz)/2.)/440.
        pos=[0.12,0.12+yoffs,0.98,0.98-yoffs]
      endif
      if sz[2] gt sz[1] then begin
        xz=sz[1]*(512./sz[2]) & xoffs=((512.-xz)/2.)/512.
        pos=[0.12+xoffs,0.12,0.98-xoffs,0.98]
      endif
      
      plot_map, pmap, dmin=plot_scl[0],dmax=plot_scl[1],window=!D.Window, position=pos, grid_spacing=plot_scl[2], tit=' '
      if custom eq 0 then begin
      emap=binmaps[4] & emap.data[where(binmaps[0].data eq 1,/null)]=0
      plot_map, emap, /over, /cont, color=cgcolor('blue'),/overlay, contour=1,/cell_fill,levels=[1]
      endif
      plot_map, binmaps[2], /over, /cont, color=cgcolor('red'),c_thick=plot_scl[3], c_linestyle=0
      
      map2index, binmap, bndex
      xyouts, 0.5,0.01,strmid(bndex.date_obs,0,16), color=cgcolor('white'), charsize=1.5, charthick=1.5,/norm, alignment=0.5
      dcord=!D & xcord=!X & ycord=!Y

      WIDGET_CONTROL, ids.opt_main,/destroy

    end
  endcase
end

;+
;******************************************************************************************
; NAME: magproperties2txt
;
; Call:
;       magproperties2txt, in, path
;
; PURPOSE:
;        Prints CATCH coronal hole magnetic field properties to file
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

pro magproperties2txt, in, path, obs, obs_date
  common version, version
  close,/all
  openw,1,path

  in[5,12]=in[5,12]/1e10
  in[6,12]=in[6,12]/1e10
  in[5,0]=in[5,0]/1e20
  in[6,0]=in[6,0]/1e20
  in[5,1]=in[5,1]/1e20
  in[6,1]=in[6,1]/1e20

  in[where(finite(in) eq 0.,/null)]=9999
  in[where(in eq 0.,/null)]=9999

  printf, 1, ';### Coronal hole properties extracted from Magnetogram ###'
  printf, 1, ';### CATCH Version '+version+' ###'
  printf, 1, ';Observer   Date Observed    Area [10^10 km^2]    Area Sigma [10^10 km^2]     Signed Mean Magnetic Field Strength [G]    Signed Mean Magnetic Field Strength Sigma [G]      '+$
       'Unsigned Mean Magnetic Field Strength [G]     Unsigned Mean Magnetic Field Strength [G]   Signed Magnetic Flux [10^20 Mx]   Signed Magnetic Flux Sigma [10^20 Mx]       '+$
       'Unsigned Magnetic Flux [10^20 Mx]   Unsigned Magnetic Flux Sigma [10^20 Mx]    Flux Balance [%]   Flux Balance Sigma [%]    Magnetic Field Skewness [ ]   Magnetic Field Skewness Sigma [ ]     '+$
       'Strong Flux Tube Count [Nr]   Strong Flux Tube Count Sigma [Nr]   Strong Flux Tube Flux Ratio [%]   Strong Flux Tube Flux Ratio Sigma [%]   Strong Flux Tube Area Ratio [%]   Strong Flux Tube Area Ratio Sigma [%]'+$
       'Weak Flux Tube Count [Nr]   Weak Flux Tube Count Sigma [Nr]   Weak Flux Tube Flux Ratio [%]   Weak Flux Tube Flux Ratio Sigma [%]   Weak Flux Tube Area Ratio [%]   Weak Flux Tube Area Ratio Sigma [%]'
  printf, 1,   obs+'  '+obs_date+'  '+strtrim(STRING(in[5,12], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,12], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,2], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,2], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,3], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,3], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,0], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,0], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,1], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,1], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,4], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,4], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,5], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,5], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,6], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,6], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,7], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,7], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,8], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,8], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,9], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,9], format='(D15.5)'),2)+'  '+strtrim(STRING(in[5,10], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,10], format='(D15.5)'),2)+'  '+$
    strtrim(STRING(in[5,11], format='(D15.5)'),2)+'  '+strtrim(STRING(in[6,11], format='(D15.5)'),2)
  printf, 1, ';##############################'
  printf, 1, ';### You may read this with idl using the following ssw routine:'
  printf, 1, ';### readcol, file, x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,$'
  printf, 1, ';### format='+string(39B)+'(A,A,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5,D15.5)'+string(39B)+',STRINGSKIP='+string(39B)+';'+string(39B)
  printf, 1, ';##############################'

  close, /all
end
