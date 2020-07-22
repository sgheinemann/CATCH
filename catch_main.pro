;##########################################################################


;+
;*********************************************************************************
; NAME: read_ini_catch
;
;
; Call:
;       read_ini_catch, filename, struct
;
; PURPOSE:
;        read .ini file for CATCH configuration
;
;
; INPUTS:
;       filename         Fileppath of .ini CATCH file
;
; OUTPUTS:
;       CATCH config variable in form of a structure
;
;
; MODIFICATION HISTORY:
;        Written by: Stephan Heinemann, March 26, 2019
;
;*******************************************************************************************

PRO read_ini_catch, filename, struct

  common general, id, paths,dir,debug

  ;# read lines from ini file
  nlines = file_lines(filename)
  lines = strarr(nlines)
  openr, lun, filename, /get_lun
  readf, lun, lines
  free_lun, lun

  inidata=list()
  foreach line, lines, l do if stregex(line, '^[[:space:]]*[;#]', /boolean) eq 0 then inidata.add,  STRSPLIT(line, /EXTRACT)
  ini=inidata.toarray()

  ;# check each value
  ;paths

  for check=0, 3 do begin
    if valid_num(ini[check,1]) eq 0 then begin
      if file_test(ini[check,1],/directory) eq 0 then begin
        ini[check,1]=dir+'/'
        print,'% ERROR in .ini file encountered; invalid path detected.'
      endif
    endif else begin
      ini[check,1]=dir
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endelse
  endfor
  ;save maps and plots
  for check=0,7 do begin
    if valid_num(ini[4+check,1]) eq 0 then begin
      if ini[4+check,1] ne 'y' and ini[4+check,1] ne 'n' then begin
        ini[4+check,1]='y'
        print,'% ERROR in .ini file encountered, default value subsituted.'
      endif
    endif else begin
      ini[4+check,1]='y'
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endelse
  endfor

  ;gridsize
  if valid_num(ini[12,1]) eq 1 then begin
    if fix(ini[12,1]) lt 0 and fix(ini[12,1]) gt 360 then begin
      ini[12,1]=strtrim(string(180),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[12,1]=strtrim(string(180),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;contoursize
  if valid_num(ini[13,1]) eq 1 then begin
    if fix(ini[13,1]) lt 1 and fix(ini[13,1]) gt 50 then begin
      ini[13,1]=strtrim(string(1),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[13,1]=strtrim(string(1),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;### dmin/dmax
  ;aia193
  if valid_num(ini[14,1]) eq 1 and valid_num(ini[15,1]) eq 1 then begin
    if fix(ini[15,1]) lt fix(ini[14,1]) then begin
      ini[14,1]=strtrim(string(20),2)
      ini[15,1]=strtrim(string(2100),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[14,1]=strtrim(string(20),2)
    ini[15,1]=strtrim(string(2100),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;eit195
  if valid_num(ini[16,1]) eq 1 and valid_num(ini[17,1]) eq 1 then begin
    if fix(ini[17,1]) lt fix(ini[16,1]) then begin
      ini[16,1]=strtrim(string(7),2)
      ini[17,1]=strtrim(string(800),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[16,1]=strtrim(string(7),2)
    ini[17,1]=strtrim(string(800),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;stereo195
  if valid_num(ini[18,1]) eq 1 and valid_num(ini[19,1]) eq 1 then begin
    if fix(ini[19,1]) lt fix(ini[18,1]) then begin
      ini[18,1]=strtrim(string(10),2)
      ini[19,1]=strtrim(string(1000),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[18,1]=strtrim(string(10),2)
    ini[19,1]=strtrim(string(1000),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;mdi
  if valid_num(ini[20,1]) eq 1 and valid_num(ini[21,1]) eq 1 then begin
    if fix(ini[21,1]) lt fix(ini[20,1]) then begin
      ini[20,1]=strtrim(string(-100),2)
      ini[21,1]=strtrim(string(100),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[20,1]=strtrim(string(-100),2)
    ini[21,1]=strtrim(string(100),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;hmi
  if valid_num(ini[22,1]) eq 1 and valid_num(ini[23,1]) eq 1 then begin
    if fix(ini[23,1]) lt fix(ini[22,1]) then begin
      ini[22,1]=strtrim(string(-50),2)
      ini[23,1]=strtrim(string(50),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[22,1]=strtrim(string(-50),2)
    ini[23,1]=strtrim(string(50),2)
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  ;resolution
  resv=[256,512,1024,2048,4096]
  for j=0, 1 do begin
    if valid_num(ini[24+j,1]) eq 1 then begin
      if n_elements(where(resv eq fix(ini[24+j,1]),/null))  eq 0 then begin
        ini[24+j,1]=strtrim(string(1024),2)
        print,'% ERROR in .ini file encountered, default value subsituted.'
      endif
    endif else begin
      ini[24+j,1]=strtrim(string(1024),2)
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endelse
  endfor


  if valid_num(ini[26,1]) eq 0 then begin
    if ini[26,1] ne 'on' and ini[26,1] ne 'off' then begin
      ini[26,1]='on'
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[26,1]='off'
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  if valid_num(ini[27,1]) eq 0 then begin
    if ini[27,1] ne 'on' and ini[27,1] ne 'off' then begin
      ini[27,1]='off'
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[27,1]='off'
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  if valid_num(ini[28,1]) eq 0 then begin
    if ini[28,1] ne 'on' and ini[28,1] ne 'off' then begin
      ini[28,1]='on'
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[28,1]='off'
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  if valid_num(ini[29,1]) eq 0 then begin
    if ini[29,1] ne 'on' and ini[29,1] ne 'off' then begin
      ini[29,1]='on'
      print,'% ERROR in .ini file encountered, default value subsituted.'
    endif
  endif else begin
    ini[29,1]='off'
    print,'% ERROR in .ini file encountered, default value subsituted.'
  endelse

  struct={config, euvpath:ini[0,1],magpath:ini[1,1],dlpath:ini[2,1],outpath:ini[3,1],smaps_euv:ini[4,1],smaps_mag:ini[5,1],$
    euv_eps:ini[6,1],mag_eps:ini[7,1],ft_eps:ini[8,1],euv_png:ini[9,1],mag_png:ini[10,1],ft_png:ini[11,1],$
    gridsize:fix(ini[12,1]),cthick:fix(ini[13,1]),aia_range:[long(ini[14,1]),long(ini[15,1])],$
    eit_range:[long(ini[16,1]),long(ini[17,1])],stereo_range:[long(ini[18,1]),long(ini[19,1])],$
    mdi_range:[long(ini[20,1]),long(ini[21,1])],hmi_range:[long(ini[22,1]),long(ini[23,1])],$
    res_euv:fix(ini[24,1]),res_mag:fix(ini[25,1]),uncert:ini[26,1],lbc:ini[27,1],free:ini[28,1],lock:ini[29,1]}


end


;+
;*********************************************************************************
; NAME: write_ini_catch
;
;
; Call:
;       write_ini_catch,  filename, struct,parent_id, /struct_old, /create_default
;
; PURPOSE:
;        create/update .ini file for CATCH configuration
;
;
; INPUTS:
;       struct           CATCH config variable in form of a structure
;       filename         Filepath of .ini CATCH file
;       parent_id        Widget ID of parent Widget
;
; KEYWORDS:
;       struct_old       if set, shows the changes from the old to the new struct and asks for approval
;       create_default   if set, create default configuration file, renames old config file as _old if exists; if '*_old.ini* exists it will be overwritten
;                        ignores struct_old and struct
;
; OUTPUTS:
;       updates or creates ini.file
;
;
; MODIFICATION HISTORY:
;        Written by: Stephan Heinemann, March 26, 2019
;
;*******************************************************************************************

PRO write_ini_catch, filename, struct,parent_id, struct_old=struct_old, create_default=create_default

  common version, version
  common general, id, paths,dir,debug

  if keyword_set(create_default) eq 0 then begin

    if keyword_set(struct_old) eq 1 then begin

      outputvec=['                                   ',$
        '   The following changes will be   ',$
        '   made to the configuration file: ',$
        '                                   ']
      testvec=outputvec

      if struct.euvpath ne paths.euvpath then outputvec=[outputvec,' EUV DATA PATH: '+paths.euvpath+'  -->>  ' +struct.euvpath+' ']
      if struct.magpath ne paths.magpath then outputvec=[outputvec,' MAG DATA PATH: '+paths.magpath+'  -->>  ' +struct.magpath+' ']
      if struct.dlpath ne paths.dlpath then outputvec=[outputvec,' DOWNLOAD PATH: '+paths.dlpath+'  -->>  ' +struct.dlpath+' ']
      if struct.outpath ne paths.outpath then outputvec=[outputvec,' RESULTS PATH: '+paths.outpath+'  -->>  ' +struct.outpath+' ']
      if struct.smaps_euv ne paths.smaps_euv then outputvec=[outputvec,' SAVE EUV MAPS: '+paths.smaps_euv+'  -->>  ' +struct.smaps_euv+' ']
      if struct.smaps_mag ne paths.smaps_mag then outputvec=[outputvec,' SAVE MAG MAPS: '+paths.smaps_mag+'  -->>  ' +struct.smaps_mag+' ']
      if struct.euv_eps ne paths.euv_eps then outputvec=[outputvec,' SAVE EUV IMAGE (.eps): '+paths.euv_eps+'  -->>  ' +struct.euv_eps+' ']
      if struct.mag_eps ne paths.mag_eps then outputvec=[outputvec,' SAVE MAG IMAGE (.eps): '+paths.mag_eps+'  -->>  ' +struct.mag_eps+' ']
      if struct.ft_eps ne paths.ft_eps then outputvec=[outputvec,' SAVE FT IMAGE (.eps): '+paths.ft_eps+'  -->>  ' +struct.ft_eps+' ']
      if struct.euv_png ne paths.euv_png then outputvec=[outputvec,' SAVE EUV IMAGE (.png): '+paths.euv_png+'  -->>  ' +struct.euv_png+' ']
      if struct.mag_png ne paths.mag_png then outputvec=[outputvec,' SAVE MAG IMAGE (.png): '+paths.mag_png+'  -->>  ' +struct.mag_png+' ']
      if struct.ft_png ne paths.ft_png then outputvec=[outputvec,' SAVE FT IMAGE (.png): '+paths.ft_png+'  -->>  ' +struct.ft_png+' ']
      if struct.gridsize ne paths.gridsize then outputvec=[outputvec,' GRIDSIZE: '+strtrim(string(paths.gridsize),2)+'  -->>  ' +strtrim(string(struct.gridsize),2)+' ']
      if struct.cthick ne paths.cthick then outputvec=[outputvec,' CONTOUR THICKNESS: '+strtrim(string(paths.cthick),2)+'  -->>  ' +strtrim(string(struct.cthick),2)+' ']
      if struct.aia_range[0] ne paths.aia_range[0] then outputvec=[outputvec,' AIA Dmin: '+strtrim(string(paths.aia_range[0]),2)+'  -->>  ' +strtrim(string(struct.aia_range[0]),2)+' ']
      if struct.aia_range[1] ne paths.aia_range[1] then outputvec=[outputvec,' AIA Dmax: '+strtrim(string(paths.aia_range[1]),2)+'  -->>  ' +strtrim(string(struct.aia_range[1]),2)+' ']
      if struct.eit_range[0] ne paths.eit_range[0] then outputvec=[outputvec,' EIT Dmin: '+strtrim(string(paths.eit_range[0]),2)+'  -->>  ' +strtrim(string(struct.eit_range[0]),2)+' ']
      if struct.eit_range[1] ne paths.eit_range[1] then outputvec=[outputvec,' EIT Dmax: '+strtrim(string(paths.eit_range[1]),2)+'  -->>  ' +strtrim(string(struct.eit_range[1]),2)+' ']
      if struct.stereo_range[0] ne paths.stereo_range[0] then outputvec=[outputvec,' STEREO Dmin: '+strtrim(string(paths.stereo_range[0]),2)+'  -->>  ' +strtrim(string(struct.stereo_range[0]),2)+' ']
      if struct.stereo_range[1] ne paths.stereo_range[1] then outputvec=[outputvec,' STEREO Dmax: '+strtrim(string(paths.stereo_range[1]),2)+'  -->>  ' +strtrim(string(struct.stereo_range[1]),2)+' ']
      if struct.mdi_range[0] ne paths.mdi_range[0] then outputvec=[outputvec,' MDI Dmin: '+strtrim(string(paths.mdi_range[0]),2)+'  -->>  ' +strtrim(string(struct.mdi_range[0]),2)+' ']
      if struct.mdi_range[1] ne paths.mdi_range[1] then outputvec=[outputvec,' MDI Dmax: '+strtrim(string(paths.mdi_range[1]),2)+'  -->>  ' +strtrim(string(struct.mdi_range[1]),2)+' ']
      if struct.hmi_range[0] ne paths.hmi_range[0] then outputvec=[outputvec,' HMI Dmin: '+strtrim(string(paths.hmi_range[0]),2)+'  -->>  ' +strtrim(string(struct.hmi_range[0]),2)+' ']
      if struct.hmi_range[1] ne paths.hmi_range[1] then outputvec=[outputvec,' HMI Dmax: '+strtrim(string(paths.hmi_range[1]),2)+'  -->>  ' +strtrim(string(struct.hmi_range[1]),2)+' ']
      if struct.res_euv ne paths.res_euv then outputvec=[outputvec,' EUV RESOLUTION: '+strtrim(string(paths.res_euv),2)+'  -->>  ' +strtrim(string(struct.res_euv),2)+' ']
      if struct.res_mag ne paths.res_mag then outputvec=[outputvec,' MAG RESOLUTION: '+strtrim(string(paths.res_mag),2)+'  -->>  ' +strtrim(string(struct.res_mag),2)+' ']
      if struct.uncert ne paths.uncert then outputvec=[outputvec,' UNCERTAINTY CALCULATIONS: '+paths.uncert+'  -->>  ' +struct.uncert+' ']
      if struct.lbc ne paths.lbc then outputvec=[outputvec,' LBC: '+paths.lbc+'  -->>  ' +struct.lbc+' ']
      if struct.free ne paths.free then outputvec=[outputvec,' Free: '+paths.free+'  -->>  ' +struct.free+' ']
      if struct.LOCK ne paths.lock then outputvec=[outputvec,' LOCK CONFIG: '+paths.lock+'  -->>  ' +struct.lock+' ']

      if n_elements(testvec) eq n_elements(outputvec) then begin
        res=dialog_message(['                                         ',$
          '  No changes were made, discarding....   ',$
          '                                         '], dialog_parent=parent_id)
        return
      endif
      outputvec=[outputvec,'                                   ',$
        '     Do you want to continue?      ',$
        '                                   ']

      res=dialog_message(outputvec, dialog_parent=parent_id,/question)
      if res eq 'No' then return
    endif

    close,/all
    openw,1,filename

    printf,1,';##### CATCH config file #####'
    printf,1,';##### Version '+version+' #####'
    printf,1,';#############################'
    printf,1,';### Specify paths for input and output (path_to_euv_data is the default for the data download widget; output_path= CATCH directory)###'
    printf,1,'path_to_euv_data:'+'  '+struct.euvpath
    printf,1,'path_to_mag_data:'+'  '+struct.magpath
    printf,1,'download_path:'+'  '+struct.dlpath
    printf,1,'output_path:'+'  '+struct.outpath
    printf,1,';#######'
    printf,1,';############################# '
    printf,1,';### save maps in save file for further processing or easy restoring (y/n)  ###'
    printf,1,'save_euv_maps:'+'  '+struct.smaps_euv
    printf,1,'save_mag_maps:'+'  '+struct.smaps_mag
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### make .eps plots of the coronal hole (y/n) ###'
    printf,1,'euv_plot:'+'  '+struct.euv_eps
    printf,1,'mag_plot:'+'  '+struct.mag_eps
    printf,1,'ft_plot:'+'  '+struct.ft_eps
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### make .png plots of the coronal hole (y/n) ###'
    printf,1,'euv_plot:'+'  '+struct.euv_png
    printf,1,'mag_plot:'+'  '+struct.mag_png
    printf,1,'ft_plot:'+'  '+struct.ft_png
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### default configuration for plots ###'
    printf,1,'gridsize:'+'  '+strtrim(string(struct.gridsize),2)
    printf,1,'contour_thickness:'+'  '+strtrim(string(struct.cthick),2)
    printf,1,';#AIA193'
    printf,1,'dmin:'+'  '+strtrim(string(struct.aia_range[0]),2)
    printf,1,'dmax:'+'  '+strtrim(string(struct.aia_range[1]),2)
    printf,1,';#EIT195'
    printf,1,'dmin:'+'  '+strtrim(string(struct.eit_range[0]),2)
    printf,1,'dmax:'+'  '+strtrim(string(struct.eit_range[1]),2)
    printf,1,';#STEREO195'
    printf,1,'dmin:'+'  '+strtrim(string(struct.stereo_range[0]),2)
    printf,1,'dmax:'+'  '+strtrim(string(struct.stereo_range[1]),2)
    printf,1,';#MDI Magnetogram'
    printf,1,'dmin:'+'  '+strtrim(string(struct.mdi_range[0]),2)
    printf,1,'dmax:'+'  '+strtrim(string(struct.mdi_range[1]),2)
    printf,1,';#HMI Magnetogram'
    printf,1,'dmin:'+'  '+strtrim(string(struct.hmi_range[0]),2)
    printf,1,'dmax:'+'  '+strtrim(string(struct.hmi_range[1]),2)
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### default configuration for for loading data ###'
    printf,1,';# possible values: 256,512,1024,2048,4096'
    printf,1,'euv_resolution:'+'  '+strtrim(string(struct.res_euv),2)
    printf,1,'mag_resolution:'+'  '+strtrim(string(struct.res_mag),2)
    printf,1,';#Uncertainty calculations default status (on/off)'
    printf,1,'uncertainty_calculation:'+'  '+struct.uncert
    printf,1,';#LBC default status (on/off)'
    printf,1,'lbc:'+'  '+struct.lbc
    printf,1,';#Free Input (on/off)'
    printf,1,'free:'+'  '+struct.free
    printf,1,';######'
    printf,1,';#############################
    printf,1,';### lock configuration file ###
    printf,1,'lock:'+'  '+struct.lock
    printf,1,';######
    close, /all


  endif else begin

    if file_test(filename) eq 1 then begin
      dire=file_dirname(filename)
      name=file_basename(filename,'.ini')
      file_move, filename,dire+'/'+name+'_old.ini',/overwrite
    endif

    close,/all
    openw,1,filename

    printf,1,';##### CATCH config file #####'
    printf,1,';##### Version '+version+' #####'
    printf,1,';#############################'
    printf,1,';### Specify paths for input and output (path_to_euv_data is the default for the data download widget; output_path= CATCH directory)###'
    printf,1,'path_to_euv_data:  '+getenv('HOME')+'/'
    printf,1,'path_to_mag_data:  '+getenv('HOME')+'/'
    printf,1,'donwnload_path:  '+getenv('HOME')+'/'
    printf,1,'output_path:  '+getenv('HOME')+'/'
    printf,1,';#######'
    printf,1,';############################# '
    printf,1,';### save maps in save file for further processing or easy restoring (y/n)  ###'
    printf,1,'save_euv_maps:'+'  y'
    printf,1,'save_mag_maps:'+'  y'
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### make .eps plots of the coronal hole (y/n) ###'
    printf,1,'euv_plot:'+'  y'
    printf,1,'mag_plot:'+'  y'
    printf,1,'ft_plot:'+'  y'
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### make .png plots of the coronal hole (y/n) ###'
    printf,1,'euv_plot:'+'  y'
    printf,1,'mag_plot:'+'  y'
    printf,1,'ft_plot:'+'  y'
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### default configuration for plots ###'
    printf,1,'gridsize:'+'  180'
    printf,1,'contour_thickness:'+'  1'
    printf,1,';#AIA193'
    printf,1,'dmin:'+'  20'
    printf,1,'dmax:'+'  2100'
    printf,1,';#EIT195'
    printf,1,'dmin:'+'  7'
    printf,1,'dmax:'+'  800'
    printf,1,';#STEREO195'
    printf,1,'dmin:'+'  10'
    printf,1,'dmax:'+'  1000'
    printf,1,';#MDI Magnetogram'
    printf,1,'dmin:'+'  -100'
    printf,1,'dmax:'+'  100'
    printf,1,';#HMI Magnetogram'
    printf,1,'dmin:'+'  -50'
    printf,1,'dmax:'+'  50'
    printf,1,';######'
    printf,1,';#############################'
    printf,1,';### default configuration for for loading data ###'
    printf,1,';# possible values: 256,512,1024,2048,4096'
    printf,1,'euv_resolution:'+'  1024'
    printf,1,'mag_resolution:'+'  1024'
    printf,1,';#Uncertainty calculations default status (on/off)'
    printf,1,'uncertainty_calculation:'+'  on'
    printf,1,';#LBC default status (on/off)'
    printf,1,'lbc:'+'  off'
    printf,1,';#Free Input (on/off)'
    printf,1,'free:'+'  off'
    printf,1,';######'
    printf,1,';#############################
    printf,1,';### lock configuration file ###
    printf,1,'lock:'+'  on'
    printf,1,';######
    close, /all

  endelse
 end 



PRO main_menu_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common conf, cid

  widget_control, ev.id, get_uvalue=uvalue                        ; get the uvalue

  CASE uvalue OF
    'ex' : begin
      ex_widget,ev
    end
    'dl' :  begin
      dl_widget,ev
    end
    'mag' :  begin
      mag_widget,ev
    end
    'config' :  begin
      widget_control, id.main, TLB_GET_OFFSET=offs
      config_main = widget_base(title='CATCH: Properties', xsize=650, ysize=640,SCR_XSIZE=650,SCR_YSIZE=640,modal=(1-debug), group_leader=id.main, xoffset=offs[0]+5, yoffset=offs[1]-175, uvalue='conftex')

      top_paths = Widget_Base(config_main, XOFFSET=220)
      label = Widget_Label(top_paths, value='Paths', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      cpaths = Widget_Base(top_paths, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=420, ysize=170)
      label = Widget_Label(top_paths, value='Paths', XOFFSET=10)

      euv_path=widget_button(cpaths,value='EUV Data Directory:',uval='euv_path', /no_release, xsize=150, xoffset=10, yoffset=20,ysize=25, tooltip='Data Directory for EUV data')
      euv_path_text=widget_TEXT(cpaths,value=paths.euvpath, xsize=37, XOFFSET=170,yoffset=17, ysize=1, uvaL='euv_text', units=0,/editable)

      mag_path=widget_button(cpaths,value='MAG DATA Directory:',uval='mag_path', /no_release, xsize=150, xoffset=10, yoffset=55,ysize=25, tooltip='Data Directory for Magnetogram data')
      mag_path_text=widget_TEXT(cpaths,value=paths.magpath, xsize=37, XOFFSET=170,yoffset=52, ysize=1, uvaL='mag_text', units=0,/editable)

      dl_path=widget_button(cpaths,value='Download Directory:',uval='dl_path', /no_release, xsize=150, xoffset=10, yoffset=90,ysize=25, tooltip='Directory for downloading data')
      dl_path_text=widget_TEXT(cpaths,value=paths.dlpath, xsize=37, XOFFSET=170,yoffset=87, ysize=1, uvaL='dl_text', units=0,/editable)

      save_path=widget_button(cpaths,value='CATCH Directory:',uval='save_path', /no_release, xsize=150, xoffset=10, yoffset=125,ysize=25, tooltip='Directory for the CATCH results')
      save_path_text=widget_TEXT(cpaths,value=paths.outpath, xsize=37, XOFFSET=170,yoffset=122, ysize=1, uvaL='save_text', units=0,/editable)


      top_saves = Widget_Base(config_main, XOFFSET=220,yoffset=190)
      label = Widget_Label(top_saves, value='Saves', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      csaves = Widget_Base(top_saves, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=200, ysize=235,/nonexclusive)
      label = Widget_Label(top_saves, value='Saves', XOFFSET=10)

      savmap1= widget_button(csaves,value='Save EUV maps (.sav)', uval='save', tooltip='Enable saving the maps used for the CH extraction calculations in an IDL save file')
      savmap2= widget_button(csaves,value='Save MAG maps (.sav)', uval='save', tooltip='Enable saving the maps used for the magnetic field calculations in an IDL save file')
      saveps1= widget_button(csaves,value='Save EUV Images (.eps)', uval='save', tooltip='Enable producing EPS plots of the EUV map, one full disk and one cutout around the CH')
      saveps2= widget_button(csaves,value='Save MAG Images (.eps)', uval='save', tooltip='Enable producing EPS plots of the MAG map, one full disk and one cutout around the CH')
      saveps3= widget_button(csaves,value='Save FT Images  (.eps)', uval='save', tooltip='Enable producing an EPS plot of the EUV map with FTs overlayed for a cutout around the CH')
      savpng1= widget_button(csaves,value='Save EUV Images (.png)', uval='save', tooltip='Enable producing PNG plots of the EUV map, one full disk and one cutout around the CH')
      savpng2= widget_button(csaves,value='Save MAG Images (.png)', uval='save', tooltip='Enable producing PNG plots of the MAG map, one full disk and one cutout around the CH')
      savpng3= widget_button(csaves,value='Save FT Images  (.png)', uval='save', tooltip='Enable producing an PNG plot of the EUV map with FTs overlayed for a cutout around the CH')

      if paths.smaps_euv eq 'y' then Widget_Control, savmap1, Set_Button=1
      if paths.smaps_mag eq 'y' then Widget_Control, savmap2, Set_Button=1
      if paths.euv_eps eq 'y' then Widget_Control, saveps1, Set_Button=1
      if paths.mag_eps eq 'y' then Widget_Control, saveps2, Set_Button=1
      if paths.ft_eps eq 'y' then Widget_Control, saveps3, Set_Button=1
      if paths.euv_png eq 'y' then Widget_Control, savpng1, Set_Button=1
      if paths.mag_png eq 'y' then Widget_Control, savpng2, Set_Button=1
      if paths.ft_png eq 'y' then Widget_Control, savpng3, Set_Button=1


      top_plot = Widget_Base(config_main, XOFFSET=10);,yoffset=10)
      label = Widget_Label(top_plot, value='Plot Options', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      cplot = Widget_Base(top_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=200, ysize=620)
      label = Widget_Label(top_plot, value='Plot Options', XOFFSET=10)

      label_grid = WIDGET_LABEL(cplot, XSIZE=110, VALUE='Gridsize', xoffset=15, yoffset=15, /align_left)
      grid_text=widget_TEXT(cplot,value=strtrim(string(paths.gridsize,format='(I)'),2), xsize=7, XOFFSET=130,yoffset=10, ysize=1, uvaL='grid_text', units=0,/editable,/ALL_EVENTS)
      label_thick = WIDGET_LABEL(cplot, XSIZE=110, VALUE='Contour Thickness', xoffset=10, yoffset=55, /align_left)
      thick_text=widget_TEXT(cplot,value=strtrim(string(paths.cthick,format='(I)'),2), xsize=7, XOFFSET=130,yoffset=50, ysize=1, uvaL='thick_text', units=0,/editable,/ALL_EVENTS)

      aia_plot = Widget_Base(cplot, XOFFSET=10,yoffset=90)
      label = Widget_Label(aia_plot, value='AIA 193', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      aiaplot = Widget_Base(aia_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=180, ysize=90)
      label = Widget_Label(aia_plot, value='AIA 195', XOFFSET=10)

      label_dmin = WIDGET_LABEL(aiaplot, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text_aia=widget_TEXT(aiaplot,value=strtrim(string(paths.aia_range[0],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=10, ysize=1, uvaL='dmin_text_aia', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(aiaplot, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text_aia=widget_TEXT(aiaplot,value=strtrim(string(paths.aia_range[1],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=50, ysize=1, uvaL='dmax_text_aia', units=0,/editable,/ALL_EVENTS)

      eit_plot = Widget_Base(cplot, XOFFSET=10,yoffset=195)
      label = Widget_Label(eit_plot, value='EIT 195', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      eitplot = Widget_Base(eit_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=180, ysize=90)
      label = Widget_Label(eit_plot, value='EIT 195', XOFFSET=10)

      label_dmin = WIDGET_LABEL(eitplot, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text_eit=widget_TEXT(eitplot,value=strtrim(string(paths.eit_range[0],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=10, ysize=1, uvaL='dmin_text_eit', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(eitplot, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text_eit=widget_TEXT(eitplot,value=strtrim(string(paths.eit_range[1],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=50, ysize=1, uvaL='dmax_text_eit', units=0,/editable,/ALL_EVENTS)

      stereo_plot = Widget_Base(cplot, XOFFSET=10,yoffset=300)
      label = Widget_Label(stereo_plot, value='STEREO 195', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      stereoplot = Widget_Base(stereo_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=180, ysize=90)
      label = Widget_Label(stereo_plot, value='STEREO 195', XOFFSET=10)

      label_dmin = WIDGET_LABEL(stereoplot, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text_stereo=widget_TEXT(stereoplot,value=strtrim(string(paths.stereo_range[0],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=10, ysize=1, uvaL='dmin_text_stereo', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(stereoplot, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text_stereo=widget_TEXT(stereoplot,value=strtrim(string(paths.stereo_range[1],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=50, ysize=1, uvaL='dmax_text_stereo', units=0,/editable,/ALL_EVENTS)

      mdi_plot = Widget_Base(cplot, XOFFSET=10,yoffset=405)
      label = Widget_Label(mdi_plot, value='MDI Magnetogram', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      mdiplot = Widget_Base(mdi_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=180, ysize=90)
      label = Widget_Label(mdi_plot, value='MDI Magnetogram', XOFFSET=10)

      label_dmin = WIDGET_LABEL(mdiplot, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text_mdi=widget_TEXT(mdiplot,value=strtrim(string(paths.mdi_range[0],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=10, ysize=1, uvaL='dmin_text_mdi', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(mdiplot, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text_mdi=widget_TEXT(mdiplot,value=strtrim(string(paths.mdi_range[1],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=50, ysize=1, uvaL='dmax_text_mdi', units=0,/editable,/ALL_EVENTS)

      hmi_plot = Widget_Base(cplot, XOFFSET=10,yoffset=510)
      label = Widget_Label(hmi_plot, value='HMI Magnetogram', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      hmiplot = Widget_Base(hmi_plot, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=180, ysize=90)
      label = Widget_Label(hmi_plot, value='HMI Magnetogram', XOFFSET=10)

      label_dmin = WIDGET_LABEL(hmiplot, XSIZE=50, VALUE='Dmin:', xoffset=10, yoffset=15, /align_left)
      dmin_text_hmi=widget_TEXT(hmiplot,value=strtrim(string(paths.hmi_range[0],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=10, ysize=1, uvaL='dmin_text_hmi', units=0,/editable,/ALL_EVENTS)
      label_dmax = WIDGET_LABEL(hmiplot, XSIZE=50, VALUE='Dmax:', xoffset=10, yoffset=55, /align_left)
      dmax_text_hmi=widget_TEXT(hmiplot,value=strtrim(string(paths.hmi_range[1],format='(I)'),2), xsize=7, XOFFSET=90,yoffset=50, ysize=1, uvaL='dmax_text_hmi', units=0,/editable,/ALL_EVENTS)



      top_handling = Widget_Base(config_main, XOFFSET=440,yoffset=190)
      label = Widget_Label(top_handling, value='Handling', XOFFSET=10)
      labelGeometry = Widget_Info(label, /GEOMETRY)
      labelYSize =  labelGeometry.ysize
      chandling = Widget_Base(top_handling, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=200, ysize=235)
      label = Widget_Label(top_handling, value='Handling', XOFFSET=10)

      reso_euv=widget_label(chandling,value=  'EUV Resolution', uval='reso_euv', xsize= 90, xoffset=10, yoffset=15,ysize=25,/align_left)
      cbox_reso_euv=widget_combobox(chandling,value=['256x256','512x512' ,'1024x1024','2048x2048','4096x4096'], uval='cbox_reso_euv', xsize= 90, xoffset=100, yoffset=15,ysize=25)

      reso_mag=widget_label(chandling,value=  'MAG Resolution', uval='reso_mag', xsize= 90, xoffset=10, yoffset=50,ysize=25,/align_left)
      cbox_reso_mag=widget_combobox(chandling,value=['256x256','512x512' ,'1024x1024','2048x2048','4096x4096'], uval='cbox_reso_mag', xsize= 90, xoffset=100, yoffset=50,ysize=25)

      case paths.res_euv of
        256:COMBOBOX_def=0
        512:COMBOBOX_def=1
        1024:COMBOBOX_def=2
        2048:COMBOBOX_def=3
        4096:COMBOBOX_def=4
      endcase
      widget_control, cbox_reso_euv, set_combobox_select=COMBOBOX_def

      case paths.res_mag of
        256:COMBOBOX_def=0
        512:COMBOBOX_def=1
        1024:COMBOBOX_def=2
        2048:COMBOBOX_def=3
        4096:COMBOBOX_def=4
      endcase
      widget_control, cbox_reso_mag, set_combobox_select=COMBOBOX_def

      chandlingb = Widget_Base(chandling, xsize=180, ysize=120,/nonexclusive, yoffset=100, xoffset=10)

      uncert= widget_button(chandlingb,value='Uncertainty calculations ', uval='uncert', tooltip='Enable uncertainty calculations for coronal hole extraction and parameter calculation', xsize=120)
      lbc= widget_button(chandlingb,value='LBC ', uval='lbc', tooltip='Enable Limb Brightening Correction', xsize=120)
      lock= widget_button(chandlingb,value='Lock properties', uval='lock', tooltip='Locks the properties, disabling the overwrite option', xsize=120)
      free= widget_button(chandlingb,value='Free input', uval='free', tooltip='Disables load restrictions and data preparation for input filtergrams and magnetograms. Use at own risk.', xsize=100)


      if paths.uncert eq 'on' then Widget_Control, uncert, Set_Button=1
      if paths.lbc eq 'on' then Widget_Control, lbc, Set_Button=1
      if paths.lock eq 'on' then Widget_Control, lock, Set_Button=1
      if paths.free eq 'on' then Widget_Control, free, Set_Button=1


      abort_config= widget_button(config_main,value='Abort', uval='abort_config',/NO_release, xsize=95 , xoffset=330., yoffset=450, tooltip='Close properties window without saving',ysize=25)
      reset_config= widget_button(config_main,value='Reset', uval='reset_config',/NO_release, xsize=95 , xoffset=220., yoffset=450, tooltip='Reset configuration to default values and close properties window',ysize=25)
      apply_config= widget_button(config_main,value='Save', uval='ap_config',/NO_release, xsize=200 , xoffset=440., yoffset=450, tooltip='Save changes and close properties window',ysize=25)



      widget_control, config_main, /realize

      logo_draw_catch_conf = widget_draw(config_main, uvalue='logo_catch_config', xsize=420, ysize=137, yoffset=440+53, xoffset=220, RENDERER=1)

      WIDGET_CONTROL, logo_draw_catch_conf, GET_VALUE=logoID_catch_conf
      WSET, logoID_catch_conf



      logo_path=dir+'catch.jpg'
      read_jpeg,logo_path, logo
      logo=congrid(logo,3,420*3,137*3,/center)
      tvimage, logo
      widget_control, logo_draw_catch_conf, sensitive=0

      cid={abort:abort_config, save:apply_config, uncert:uncert,lbc:lbc,lock:lock, cbox_euv:cbox_reso_euv,cbox_mag:cbox_reso_mag,dmin_mdi:dmin_text_mdi,dmax_mdi:dmax_text_mdi,$
        dmin_hmi:dmin_text_hmi,dmin_aia:dmin_text_aia,dmin_stereo:dmin_text_stereo,dmin_eit:dmin_text_eit,dmax_eit:dmax_text_eit,dmax_hmi:dmax_text_hmi,$
        dmax_stereo:dmax_text_stereo,dmax_aia:dmax_text_aia,savemap_euv:savmap1,savemap_mag:savmap2,saveeps_euv:saveps1,saveeps_mag:saveps2,saveeps_ft:saveps3,$
        savepng_euv:savpng1,savepng_mag:savpng2,savepng_ft:savpng3,mag_path:mag_path_text,save_path:save_path_text,euv_path:euv_path_text,dl_path:dl_path_text,$
        grid_text:grid_text,thick_text:thick_text,config_main:config_main,free:free}

      xmanager, 'config',config_main,  /no_block
    end
    'exit': WIDGET_CONTROL, id.main,/destroy
  ENDCASE

END


PRO config_event, ev                                          ; event handler
  common general, id, paths,dir,debuguv
  common menu_id, menuid
  common conf, cid

  widget_control, ev.id, get_uvalue=uvalue
  CASE uvalue OF
    'dmin_text_aia':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmin_aia, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmin_aia, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered ,cid.dmin_aia
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,cid.dmin_aia
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmax_text_aia':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmax_aia, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmax_aia, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.dmax_aia
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.dmax_aia
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmin_text_eit':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmin_eit, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmin_eit, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered ,cid.dmin_eit
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,cid.dmin_eit
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmax_text_eit':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmax_eit, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmax_eit, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.dmax_eit
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.dmax_eit
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmin_text_stereo':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmin_stereo, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmin_stereo, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered ,cid.dmin_stereo
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,cid.dmin_stereo
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmax_text_stereo':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmax_stereo, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmax_stereo, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.dmax_stereo
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.dmax_stereo
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmin_text_mdi':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmin_mdi, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmin_mdi, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered ,cid.dmin_mdi
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,cid.dmin_mdi
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmax_text_mdi':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmax_mdi, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmax_mdi, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.dmax_mdi
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.dmax_mdi
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmin_text_hmi':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmin_hmi, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmin_hmi, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered ,cid.dmin_hmi
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered ,cid.dmin_hmi
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'dmax_text_hmi':begin
      if ev.Type LE 1 then begin
        Altered = 0
        ;print,ev.Offset,ev.Length
        WIDGET_CONTROL, cid.dmax_hmi, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.dmax_hmi, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.dmax_hmi
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.dmax_hmi
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
        WIDGET_CONTROL, cid.grid_text, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.grid_text, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.grid_text
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.grid_text
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
        WIDGET_CONTROL, cid.thick_text, $
          SET_TEXT_SELECT=[ev.Offset-1,ev.Length]
        WIDGET_CONTROL, cid.thick_text, SET_VALUE='', /USE_TEXT_SELECT
        if ev.type eq 0 then begin
          FIELD_INT, ev.Ch ,ev, altered,cid.thick_text
        endif else begin
          Chars= BYTE(ev.Str)
          FOR ccs=0,N_ELEMENTS(Chars)-1 DO FIELD_INT,Chars[ccs],ev, altered,cid.thick_text
        endelse
      endif ELSE begin
      endelse
      ;   All delete/add char evs effect the contents of
      ;   a string. <CR> is considered special.
      IF ev.Type GE 0 AND ev.Type LE 2 THEN Altered = 1
      IF ev.Type EQ 0 THEN $
        Altered  = 1 + (ev.Ch EQ 10b)
    end
    'euv_path':begin
      widget_control, cid.euv_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, cid.euv_path, set_value=temp_path
    end
    'mag_path':begin
      widget_control, cid.mag_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, cid.mag_path, set_value=temp_path
    end
    'dl_path':begin
      widget_control, cid.dl_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, cid.dl_path, set_value=temp_path
    end
    'save_path':begin
      widget_control, cid.save_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, cid.save_path, set_value=temp_path
    end
    'abort_config': WIDGET_CONTROL, cid.config_main,/destroy;begin &  &  widget_control, ids.ex_main, sensitive=1 & end
    'ap_config': begin
      new_paths=paths
      widget_control, cid.euv_path, get_value=temp_path & new_paths.euvpath=temp_path
      widget_control, cid.mag_path, get_value=temp_path & new_paths.magpath=temp_path
      widget_control, cid.dl_path, get_value=temp_path & new_paths.dlpath=temp_path
      widget_control, cid.save_path, get_value=temp_path & new_paths.outpath=temp_path
      if widget_info(cid.savemap_euv,/button_set) eq 1 then new_paths.smaps_euv ='y' else new_paths.smaps_euv ='n'
      if widget_info(cid.savemap_mag,/button_set) eq 1 then new_paths.smaps_mag ='y' else new_paths.smaps_mag ='n'
      if widget_info(cid.saveeps_euv,/button_set) eq 1 then new_paths.euv_eps ='y' else new_paths.euv_eps ='n'
      if widget_info(cid.saveeps_mag,/button_set) eq 1 then new_paths.mag_eps ='y' else new_paths.mag_eps ='n'
      if widget_info(cid.saveeps_ft,/button_set) eq 1 then new_paths.ft_eps ='y' else new_paths.ft_eps ='n'
      if widget_info(cid.savepng_euv,/button_set) eq 1 then new_paths.euv_png ='y' else new_paths.euv_png ='n'
      if widget_info(cid.savepng_mag,/button_set) eq 1 then new_paths.mag_png ='y' else new_paths.mag_png ='n'
      if widget_info(cid.savepng_ft,/button_set) eq 1 then new_paths.ft_png ='y' else new_paths.ft_png ='n'
      widget_control, cid.grid_text, get_value=val & new_paths.gridsize=val
      widget_control, cid.thick_text, get_value=val & new_paths.cthick=val
      widget_control, cid.dmin_aia, get_value=val1
      widget_control, cid.dmax_aia, get_value=val2 & new_paths.aia_range=[val1,val2]
      widget_control, cid.dmin_eit, get_value=val1
      widget_control, cid.dmax_eit, get_value=val2 & new_paths.eit_range=[val1,val2]
      widget_control, cid.dmin_stereo, get_value=val1
      widget_control, cid.dmax_stereo, get_value=val2 & new_paths.stereo_range=[val1,val2]
      widget_control, cid.dmin_mdi, get_value=val1
      widget_control, cid.dmax_mdi, get_value=val2 & new_paths.mdi_range=[val1,val2]
      widget_control, cid.dmin_hmi, get_value=val1
      widget_control, cid.dmax_hmi, get_value=val2 & new_paths.hmi_range=[val1,val2]
      val=WIDGET_info(cid.cbox_euv, /combobox_gettext)
      case val of
        '4096x4096': val1=4096
        '2048x2048': val1=2048
        '1024x1024': val1=1024
        '512x512'  : val1=512
        '256x256'  : val1=256
      endcase
      new_paths.res_euv=val1
      val=WIDGET_info(cid.cbox_mag, /combobox_gettext)
      case val of
        '4096x4096': val1=4096
        '2048x2048': val1=2048
        '1024x1024': val1=1024
        '512x512'  : val1=512
        '256x256'  : val1=256
      endcase
      new_paths.res_mag=val1
      if widget_info(cid.uncert,/button_set) eq 1 then new_paths.uncert ='on' else new_paths.uncert ='off'
      if widget_info(cid.lbc,/button_set) eq 1 then new_paths.lbc ='on' else new_paths.lbc ='off'
      if widget_info(cid.lock,/button_set) eq 1 then new_paths.lock ='on' else new_paths.lock ='off'
      if widget_info(cid.free,/button_set) eq 1 then new_paths.free ='on' else new_paths.free ='off'
      
      write_ini_catch, getenv('HOME')+'/.config_CATCH.ini', new_paths,id.main, /struct_old
      paths=new_paths
      WIDGET_CONTROL, cid.config_main,/destroy
    end
    'reset_config':begin
      res=dialog_message( ['                            ',$
        '  Do you want to reset the  ',$
        '   CATCH configuration to   ',$
        '    the default values?     ',$
        '                            '], dialog_parent=cid.config_main,/question)
      if res eq 'Yes' then begin
        write_ini_catch, getenv('HOME')+'/.config_CATCH.ini', 1, /create_default
        read_ini_catch,getenv('HOME')+'/.config_CATCH.ini', paths
        WIDGET_CONTROL, cid.config_main,/destroy
      endif else return
    end
    else: dummy=0
  endcase
end

;+
;******************************************************************************************
; NAME: catch_main
;
;
; CALL:
;       catch_main
;
;
; LICENSE:
;       BSD 3-Clause License (full license file included in the code directory)
;       
;       
; PURPOSE:
;        Analysis tool for Coronal Holes
;
;
; INPUTS:
;       193A/195A Filtergrams
;       Photospheric Magnetograms
;
;
; OUTPUTS:
;       Statistics, Images & Fits files
;
;
; MODIFICATION HISTORY & CHANGELOG:
;        Written by: Stephan Heinemann, May 2018   (alpha version)
;        
;        November 2018: (v1.0beta) beta version release
;        
;        March 2019: (v2.00beta) major update:         #NOTE:  backward compatibility to versions prior to v2.0beta is not tested nor guaranteed
;             -Major Changes
;                     -NEW FEATURE:   Drawing tool in the extraction widget to adjust the coronal hole mask
;                     -Fixed a significant problem with the magnetograms: magnetograms were automatically scaled to the binary map resolution --> now possible to set resolution properly
;                     -Updated the config.txt file for .png plots and directory for magnetograms
;                     -Reorganized the output
;                               -file naming changed to: 'yyyymmdd_hhmmss_OBSERVER_resolution_*additional_infos*.*extension*' to give additional information and make filenames compatible with windows
;                               -output files for one euv timestep are now saved in one directory named 'yyyymmdd_hhmmss_CATCH'
;                     -Temporarily disabled the in-situ viewing option in the extraction tool because of problems with the sswdb in-situ database
;                     -Added the option of producing .png plots in addition to .eps plots
;                     -Added an estimation for the goodness of the boundary extracted using a 3 color circle:
;                               -red: maximum derivation from the mean at +/- 2% I_median is greater than 10% 
;                               -orange: maximum derivation from the mean at +/- 2% I_median is between 5% and 10% 
;                               -green: maximum derivation from the mean at +/- 2% I_median is lower than 5% 
;                     -Reworked error calculations:
;                               -The error is the maximum deviation from the mean of the parameter calculated from the five masks created with a threshold of +/- 2% I_median.
;                                It represents how the parameter changes with variations near the threshold (lower = better). It is an estimate how good the boundary is.
;                               -When using the the Drawing Tool feature no errors are calculated                            
;             -Minor Changes                               
;                     -Path of filenames displayed in the tool has been removed to make viewing the files easier
;                     -Changed naming of options to make it more consistent:
;                               - 'OUTPUT DIRECTORY' --> 'DATA DIRECTORY'   (in the download widget)
;                               - 'INPUT DIRECTORY'  --> 'DATA DIRECTORY'   (in the extraction widget)
;                               - 'INPUT DIRECTORY'  --> 'DATA DIRECTORY'   (in the mag widget)
;                               - 'INPUT DIRECTORY'  --> 'CATCH DIRECTORY'  (in the mag widget)
;                     -Updated how the histogram is displayed in the extraction widget. Also shows the threshold in the absolute DN value
;                     -Updated the properties output textfile to include that DN value of the threshold including the new readin procedure
;                     -Added the option to modify the contour thickness in the 'Plot Options' panel
;                     -The 'Plot Options' panel now only allows Integer as input
;                     -Various bugfixing
;                              
;        March 2019: (v2.10beta):
;             -Major Changes
;                     -Reworked properties handling
;                               - Changed the configuration file from '*.txt' to '*.ini'
;                               - Added 'Properties' window accessible via the 'Main Menu' window
;                               - Properties may now configured from within CATCH and will be saved in the configuration file and may be restored
;                               - Possibility to save the current configuration of the workspace without using the 'Properties' window (disabled by default, can be enabled in the 'Properties' window)
;                               - Added more options to be modified by the user
;             -Minor Changes
;                     -Changed naming of options to make it more consistent:
;                               - 'DATA DIRECTORY' --> 'DOWNLOAD DIRECTORY'   (in the download widget)
;                     -Various bugfixing
;                     -Added license file
;
;        April 2019: (v2.11beta):
;             -Major Changes
;                     -New CATCH Logo
;             -Minor Changes
;                     -Various bugfixing
;                     
;        July 2019: (v1.00):  RELEASE
;             -Major Changes
;                     -Added the category factor to the coronal hole extraction (as described in Heinemann et al. 2019)
;                               - Displays the stability ohighf the boundar
;                                   - <1  : high stability
;                                   - 1-2 : medium stability
;                                   - >2  : low stability
;                               - Added a color box that displays the level of stability (green: high; orange: medium; red: low)
;                     -Removed the estimation for the goodness of the boundary extracted using a 3 color circle 
;             -Minor Changes
;                     -Added the option to turn oon/off the uncertainty calculations
;                     -Added the CATCH logo to .eps full disk output files from coronal holes extraction and magnetic field analysis
;                     -Changed the displayed uncertainties to from contours to shaded areas (blue)
;                     -Various bugfixing   
;                      
;        August 2019: (v1.01):  
;             -Major Changes
;                     -changed the error calculations from +/- 2% to +/- 2DN 
;             -Minor Changes
;                     -Fixed CoM calculations for SOHO and STEREO; all coordinates are given in HEEQ (longitude, latitude)
;                     -Various bugfixing         
;                     
;        September 2019: (v1.02):
;             -Major Changes
;                     -Added the option to use point spread function (PSF) deconvolution on 193 AIA/SDO filtergrams before extraction
;		                  -Added the option tu use abitrary full-disk filtergrams and magnetograms as input (header must be SDO like, use at your own risk).
;             -Minor Changes
;                     -Various Bugfixing
;
;        November 2019: (v1.03):
;             -Minor Changes
;                     -Various Bugfixing for manual filtergram input (tested for AIA, SWAP, XRT full disk images)               
;
;        Dezember 2019: (v1.10):
;             -Major Change
;                     -CATCH is now available as a SSW package (http://www.lmsal.com/solarsoft/ssw_packages_info.html)
;             -Minor Changes
;                     -Moved location of the configuration file (.config_CATCH.ini) to $HOME
;                     -Adjusted other pathing
;                     -Various Bugfixing  
;                        
;        July 2020: (v1.11):
;             -Minor Changes
;                     -Various Bugfixing                                                                                   
;*******************************************************************************************
;-
;
;########################### Main Menu ####################################
pro catch_main

  ;##################### compile all sub-programs

  ;Widget Tree
  RESOLVE_ROUTINE, 'dl_widget', /COMPILE_FULL_FILE, /EITHER
  RESOLVE_ROUTINE, 'ex_widget', /COMPILE_FULL_FILE, /EITHER
  RESOLVE_ROUTINE, 'mag_widget', /COMPILE_FULL_FILE, /EITHER
  
  ;Additional Programs  
  RESOLVE_ROUTINE, 'curve_corr', /COMPILE_FULL_FILE
  RESOLVE_ROUTINE, 'calc_masscenter', /COMPILE_FULL_FILE
  RESOLVE_ROUTINE, 'extract_ch', /COMPILE_FULL_FILE
  RESOLVE_ROUTINE, 'annulus_limb_correction', /COMPILE_FULL_FILE,/is_function
  ;#####################

  ;##################### display version message
  res=dialog_message($
    ['                                                                                ',$
    '                                   C.A.T.C.H.                                   ',$
    '                                                                                ',$
    '                 Collection of Analysis Tools for Coronal Holes                 ',$
    '                                                                                ',$
    '          Version v1.11       July, 2020                                        ',$
    '                                                                                ',$
    '          Developed by:       Stephan G. Heinemann                              ',$
    '                                                                                ',$
    '          Contributors:       Manuela Temmer                                    ',$
    '                              Niko Heinemann                                    ',$
    '                              Karin Dissauer                                    ',$
    '                              Evangelia Samara                                  ',$
    '                              Veronika Jercic                                   ',$
    '                              Stefan J. Hofmeister                              ',$
    '                              Astrid Veronig                                    ',$
    '                                                                                ',$
    '          A special thanks goes to all the Testers.                             ',$
    '                                                                                ',$
    '          Before use, please read the user manual.                              ',$
    '                                                                                ',$
    '          If you have any problems, questions, ideas or suggestions,            ',$
    '          please contact the author at:  stephan.heinemann@hmail.at             ',$
    '                                                                                ',$
    '          Changelog:                                                            ',$
    '                    -RELEASE of v1.00 in July 2019                              ',$
    '                    -RELEASE as a SSW package v1.10 in Dezember 2019            ',$
    '                    -Full changelog can be found in                             ',$
    '                     the catch_main.pro file                                    ',$
    '                                                                                ',$
    '                                                                                ',$
    '                      For information please see:                               ',$     
    '                 Heinemann et al. 2019, Sol. Phys., 294, 144                    ',$
    '                                                                                ',$
    '         https://ui.adsabs.harvard.edu/abs/2019SoPh..294..144H/abstract         ',$           
    '                                                                                '],$
    title='CATCH',/information, /center)
  ;#####################

  common general, id, paths,dir,debug
  common menu_id, menuid
  common version, version
  version='v1.11'

  debug =0   ; debug mode if set to 1
  
  screen=get_screen_size()
  
  
  main = widget_base(title='CATCH', xsize=660, ysize=290,SCR_XSIZE=660,SCR_YSIZE=290, xoffset=(screen[0]-660.)/2., yoffset=(screen[1]-290.)/2.)

  findpro,'catch_main', dirlist=dir,/noprint

  main_menu = Widget_Base(main, xoffset=5)
  label = Widget_Label(main_menu, value='Main Menu', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  menu = Widget_Base(main_menu, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=640, xoffset=5, ysize=270)
  label = Widget_Label(main_menu, value='Main Menu', XOFFSET=10)


;####### Buttons
  dl=widget_button(menu,value='Download Data', uval='dl',/NO_release, xsize=200, xoffset=10, yoffset=15,ysize=25, tooltip='Data download via VSO')
  
  ex=widget_button(menu,value='Coronal Hole Extraction', uval='ex',/NO_release, xsize=200, xoffset=220, yoffset=15,ysize=25, tooltip='Coronal hole extraction based on an intensity thesholding technique')
  
  mag=widget_button(menu,value='Magnetic Field Analysis', uval='mag',/NO_release, xsize=200, xoffset=430, yoffset=15,ysize=25, tooltip='Magnetic field analysis of the global and fine structure')

  exit= widget_button(menu,value='Exit', uval='exit',/NO_release, xsize=110 , xoffset=520., yoffset=235, tooltip='Exit CATCH',ysize=25)
  
  config= widget_button(menu,value='Properties', uval='config',/NO_release, xsize=110 , xoffset=520., yoffset=205, tooltip='Configuration of CATCH',ysize=25)
  
  widget_control,/realize,main
  
  
;###### Logos  

  logo_draw_catch = widget_draw(menu, uvalue='logo_catch', xsize=500, ysize=163, yoffset=55+21, xoffset=10, RENDERER=1)
  
  WIDGET_CONTROL, logo_draw_catch, GET_VALUE=logoID_catch
  WSET, logoID_catch
  
  logo_path=dir+'catch.jpg'
  read_jpeg,logo_path, logo
  logo=congrid(logo,3,500*3,163*3,/center)
  tvimage, logo
  
  logo_draw= widget_draw(menu, uvalue='logo', xsize=100, ysize=86, yoffset=45+7, xoffset=525, RENDERER=1)
    WIDGET_CONTROL, logo_draw, GET_VALUE=logoID
  WSET, logoID


logo_path_ug=dir+'logo_uni_graz.jpg'
read_jpeg,logo_path_ug, logo_ug
logo_ug=congrid(logo_ug,3,100*2,86*2,/center)
tvimage, logo_ug
  
  
  logo_ffg_draw = widget_draw(menu, uvalue='ffglogo', xsize=100, ysize=50., yoffset=145, xoffset=525, RENDERER=1)
  
 WIDGET_CONTROL, logo_ffg_draw, GET_VALUE=logoIDffg
  WSET, logoIDffg
  logo_path=dir+'ffg_logo.png'
  read_png,logo_path, ffglogo
  ;ffglogo=congrid(ffglogo,3,200,100,/center)
  tvimage, ffglogo
  
  
  
  widget_control, logo_draw_catch, sensitive=0
  widget_control, logo_draw, sensitive=0
  widget_control, logo_ffg_draw, sensitive=0
  
;###### config file
cfile=getenv('HOME')+'/.config_CATCH.ini'
if file_test(cfile) eq 1 then begin & read_ini_catch,cfile, paths & endif else begin 
res=dialog_message( ['                                       ',$
                     '  No configuration file (.ini) found!',$
                     '   Do you want to set up a new one? ',$
                     '                                       '], dialog_parent=main,/question)
if res eq 'Yes' then begin
  write_ini_catch, cfile, 1, /create_default
  read_ini_catch,cfile, paths
endif else begin
  WIDGET_CONTROL, main,/destroy
endelse
endelse

;###### IDs

  menuid={dl:dl,ex:ex,mag:mag}
  id={main:main}

  xmanager, 'main_menu',main,  /no_block

end
