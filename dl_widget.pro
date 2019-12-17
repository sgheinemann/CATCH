;########################### Data Download ####################################
pro dl_widget, ev

  common general, id, paths,dir,debug
  common menu_id, menuid
  common dl, ids, temp_paths, date_array

  widget_control, id.main, TLB_GET_OFFSET=offs

  dl_main = widget_base(title='CATCH: Data Download', xsize=660, ysize=255,SCR_XSIZE=660,SCR_YSIZE=255,modal=(1-debug), group_leader=id.main, xoffset=offs[0], yoffset=offs[1]+17.5 )

  top = Widget_Base(dl_main, xoffset=5)
  label = Widget_Label(top, value='Data Download', XOFFSET=10)
  labelGeometry = Widget_Info(label, /GEOMETRY)
  labelYSize =  labelGeometry.ysize
  downl = Widget_Base(top, /FRAME, YOFFSET=labelYSize/2, YPAD=10, XPAD=10,xsize=640, xoffset=5, ysize=235)
  label = Widget_Label(top, value='Data Download', XOFFSET=10)

  dl_path=widget_button(downl,value='Download Directory:',uval='dl_path', /no_release, xsize=200, xoffset=10, yoffset=20,ysize=25, tooltip='Specify output directory for download')
  dl_path_text=widget_TEXT(downl,value=paths.dlpath, xsize=65, XOFFSET=220,yoffset=17, ysize=1, uvaL='dl_text', units=0,/editable)


    SOURCE=widget_label(downl,value=  'Source:', uval='source', xsize= 100, xoffset=10, yoffset=70,ysize=25,/align_left)
    cbox_source=widget_combobox(downl,value=[' ','SDO','SOHO','STA', 'STB'], uval='cbox_source', xsize= 100, xoffset=150, yoffset=70,ysize=25)
    
    data_prod=widget_label(downl,value=  'Data Product:', uval='data_prod', xsize= 100, xoffset=270, yoffset=70,ysize=25,/align_right)
    cbox_dprod=widget_combobox(downl,value=sindgen(3), uval='cbox_dprod', xsize= 200, xoffset=400, yoffset=70,ysize=25)
    widget_control, cbox_dprod, SENSITIVE=0
    
    date=widget_label(downl,value=  'Date:', uval='date', xsize= 70, xoffset=10, yoffset=110,ysize=25,/align_left)
    
    date_y=widget_label(downl,value=  'Year:', uval='year', xsize= 50, xoffset=90, yoffset=110,ysize=25,/align_right)
    cbox_y=widget_combobox(downl,value=sindgen(3), uval='cbox_y', xsize= 100, xoffset=150, yoffset=110,ysize=25)
    widget_control, cbox_y, SENSITIVE=0
    
    date_m=widget_label(downl,value=  'Month:', uval='month', xsize= 50, xoffset=270, yoffset=110,ysize=25,/align_right)
    cbox_m=widget_combobox(downl,value=sindgen(3), uval='cbox_m', xsize= 100, xoffset=330, yoffset=110,ysize=25)
    widget_control, cbox_m, SENSITIVE=0
    
    date_d=widget_label(downl,value=  'Day:', uval='day', xsize= 50, xoffset=450, yoffset=110,ysize=25,/align_right)
    cbox_d=widget_combobox(downl,value=sindgen(3), uval='cbox_d', xsize= 100, xoffset=500, yoffset=110,ysize=25)
    widget_control, cbox_d, SENSITIVE=0
    
    time=widget_label(downl,value=  'Time:', uval='time', xsize= 70, xoffset=10, yoffset=150,ysize=25,/align_left)

    date_h=widget_label(downl,value=  'Hour:', uval='hour', xsize= 50, xoffset=90, yoffset=150,ysize=25,/align_right)
    cbox_h=widget_combobox(downl,value=sindgen(3), uval='cbox_h', xsize= 100, xoffset=150, yoffset=150,ysize=25)
    widget_control, cbox_h, SENSITIVE=0

    date_m=widget_label(downl,value=  'Minute:', uval='min', xsize= 50, xoffset=270, yoffset=150,ysize=25,/align_right)
    cbox_min=widget_combobox(downl,value=sindgen(3), uval='cbox_min', xsize= 100, xoffset=330, yoffset=150,ysize=25)
    widget_control, cbox_m, SENSITIVE=0

    date_s=widget_label(downl,value=  'Second:', uval='sec', xsize= 50, xoffset=450, yoffset=150,ysize=25,/align_right)
    cbox_s=widget_combobox(downl,value=sindgen(3), uval='cbox_s', xsize= 100, xoffset=500, yoffset=150,ysize=25)
    widget_control, cbox_s, SENSITIVE=0
    
    abort= widget_button(downl,value='Done', uval='abort',/NO_release, xsize=110 , xoffset=400., yoffset=200, tooltip='Exit the download widget',ysize=25)
    exit= widget_button(downl,value='Exit', uval='exit',/NO_release, xsize=110 , xoffset=520., yoffset=200, tooltip='Exit CATCH',ysize=25)
    
    start= widget_button(downl,value='Download', uval='start',/NO_release, xsize=260 , xoffset=10., yoffset=200, tooltip='Start downloading',ysize=25)
    query= widget_button(downl,value='Query File', uval='query',/NO_release, xsize=110 , xoffset=280., yoffset=200, tooltip='Query the date of the download file',ysize=25)
 
  ids={dl_path:dl_path_text,cbox_source:cbox_source, cbox_dprod:cbox_dprod, cbox_y:cbox_y, cbox_m:cbox_m, cbox_d:cbox_d, cbox_h:cbox_h,dl_main:dl_main,$
       cbox_min:cbox_min, cbox_s:cbox_s, abort:abort, exit:exit, start:start, query:query}
  
  temp_path=' '
  
  restore, dir+'calendar.sav'
  widget_control,/realize,dl_main

  xmanager, 'download',dl_main,  /no_block


end


PRO download_event, ev                                          ; event handler
  common general, id, paths,dir,debug
  common menu_id, menuid
  common dl, ids, temp_paths, date_array
  
  widget_control, ev.id, get_uvalue=uvalue                        ; get the uvalue


  CASE uvalue OF
    'dl_path': begin
      widget_control, ids.dl_path, get_value=temp_path
      temp_path=dialog_pickfile(path=temp_path,/directory)
      widget_control, ids.dl_path, set_value=temp_path
    end
    'cbox_source': begin
      val=WIDGET_info(ids.cbox_source, /combobox_gettext)
     
        if val eq 'SDO'then begin
          widget_control, ids.cbox_dprod, SENSITIVE=1
          widget_control, ids.cbox_dprod, set_value=['AIA 193A','720s LOS HMI Magnetogram','45s LOS HMI Magnetogram']          
          widget_control, ids.cbox_y, SENSITIVE=1
          widget_control, ids.cbox_y, set_value=[' ',string((indgen(22)+2010))]
          endif
        if val eq 'SOHO' then begin
            widget_control, ids.cbox_dprod, SENSITIVE=1
            widget_control, ids.cbox_dprod, set_value=['EIT 195A', 'MDI 96min Magnetogram']

            widget_control, ids.cbox_y, SENSITIVE=1
            widget_control, ids.cbox_y, set_value=[' ',string((indgen(36)+1996))]
          endif
        if val eq 'STA' then begin
            widget_control, ids.cbox_dprod, SENSITIVE=0
            widget_control, ids.cbox_dprod, set_value=['EUVI 195A']

            widget_control, ids.cbox_y, SENSITIVE=1
            widget_control, ids.cbox_y, set_value=[' ',string((indgen(26)+2006))]
          endif
        if val eq 'STB' then begin
            widget_control, ids.cbox_dprod, SENSITIVE=0
            widget_control, ids.cbox_dprod, set_value=['EUVI 195A']

            widget_control, ids.cbox_y, SENSITIVE=1
            widget_control, ids.cbox_y, set_value=[' ',string((indgen(26)+2006))]
          endif
        end
        
         'cbox_y':begin
            widget_control, ids.cbox_m, SENSITIVE=1
            widget_control, ids.cbox_m, set_value=[string(indgen(12)+1)]

            widget_control, ids.cbox_d, SENSITIVE=1
            valy=WIDGET_info(ids.cbox_y, /combobox_gettext)
            valy=long(valy)
            valm=WIDGET_info(ids.cbox_m, /combobox_gettext)
            valm=long(valm)
             
            a_dex=array_indices(date_array[0,*],where(date_array[0,*] eq valy)) & aa_dex=reform(a_dex[1,*]) & aa_dex=aa_dex[uniq(aa_dex)]
            b_dex=array_indices(date_array[1,aa_dex],where(date_array[1,aa_dex] eq valm)) & bb_dex=reform(b_dex[1,*]) & bb_dex=bb_dex[uniq(bb_dex)]
            cdex= aa_dex[bb_dex]
            arr=date_array[2,cdex]
            ddy=(string(reform(fix(arr))))
            widget_control, ids.cbox_d, set_value=[ddy]
            
            widget_control, ids.cbox_h, SENSITIVE=1
            widget_control, ids.cbox_h, set_value=[string(indgen(24))]
            
            widget_control, ids.cbox_min, SENSITIVE=1
            widget_control, ids.cbox_min, set_value=[string(indgen(60))]
            
            widget_control, ids.cbox_s, SENSITIVE=1
            widget_control, ids.cbox_s, set_value=[string(indgen(60))]
          end
    
    
    'cbox_m':begin
    valy=WIDGET_info(ids.cbox_y, /combobox_gettext)
    valy=long(valy)
    valm=WIDGET_info(ids.cbox_m, /combobox_gettext)
    valm=long(valm)

            a_dex=array_indices(date_array[0,*],where(date_array[0,*] eq valy)) & aa_dex=reform(a_dex[1,*]) & aa_dex=aa_dex[uniq(aa_dex)]
            b_dex=array_indices(date_array[1,aa_dex],where(date_array[1,aa_dex] eq valm)) & bb_dex=reform(b_dex[1,*]) & bb_dex=bb_dex[uniq(bb_dex)]
            cdex= aa_dex[bb_dex]
            arr=date_array[2,cdex]
            ddy=(string(reform(fix(arr))))
            widget_control, ids.cbox_d, set_value=[ddy]
            
    end
    'cbox_d':
    'cbox_h':
    'cbox_min':
    'cbox_s':
    'cbox_dprod':
    'abort': begin
      
      IF paths.lock eq 'off' then begin
        new_paths=paths
        changes=0
        widget_control, ids.dl_path, get_value=temp_path
        if temp_path ne paths.dlpath then begin & changes+=1 & new_paths.dlpath=temp_path & endif
          if changes gt 0 then begin
          write_ini_catch, getenv('HOME')+'/.config_CATCH.ini', new_paths,ids.dl_main, /struct_old
          paths=new_paths
        endif
      ENDIF
      
       WIDGET_CONTROL, ids.dl_main,/destroy ;& widget_control, id.main, SENSITIVE=1 & endbegin & 
       end
    'exit': begin 
      IF paths.lock eq 'off' then begin
        new_paths=paths
        changes=0
        widget_control, ids.dl_path, get_value=temp_path
        if temp_path ne paths.dlpath then begin & changes+=1 & new_paths.dlpath=temp_path & endif
          if changes gt 0 then begin
          write_ini_catch, getenv('HOME')+'/.config_CATCH.ini', new_paths,ids.dl_main, /struct_old
          paths=new_paths
        endif
      ENDIF
       WIDGET_CONTROL, ids.dl_main,/destroy & WIDGET_CONTROL, id.main,/destroy 
       end
    'query': begin
      widget_control, /hourglass
      valy=WIDGET_info(ids.cbox_y, /combobox_gettext)
      valm=WIDGET_info(ids.cbox_m, /combobox_gettext)
      vald=WIDGET_info(ids.cbox_d, /combobox_gettext)
      valh=WIDGET_info(ids.cbox_h, /combobox_gettext)
      valmin=WIDGET_info(ids.cbox_min, /combobox_gettext)
      vals=WIDGET_info(ids.cbox_s, /combobox_gettext)
      search_time=strtrim(valy,2)+'-'+strtrim(valm,2)+'-'+strtrim(vald,2)+'T'+strtrim(valh,2)+':'+strtrim(valmin,2)+':'+strtrim(vals,2)
      search_time=anytim(anytim(search_time),/ccsds)
      min_time=anytim(anytim(search_time)-24.*3.*3600.,/ccsds)
      max_time=anytim(anytim(search_time)+24.*3.*3600.,/ccsds)

      val_source=WIDGET_info(ids.cbox_source, /combobox_gettext)
      vals_prod=WIDGET_info(ids.cbox_dprod, /combobox_gettext)
      case vals_prod of
       'AIA 193A': search=vso_search(near=search_time,start_time=min_time,end_time=max_time,wave='193',inst='aia',source='sdo',provider='JSOC',/quiet)
        '720s LOS HMI Magnetogram': search=vso_search(near=search_time,start_time=min_time,end_time=max_time,inst='hmi',source='sdo',PHYSOBS='vector_magnetic_field',provider='JSOC',/quiet)
        '45s LOS HMI Magnetogram':  search=vso_search(near=search_time,start_time=min_time,end_time=max_time,inst='hmi',source='sdo',PHYSOBS='LOS_magnetic_field',provider='JSOC',/quiet)
        'EIT 195A': begin
          search=vso_search(min_time,max_time,wave='195',inst='eit',source='soho',/quiet)
          if datatype(search) ne 'STC' then begin
            res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
            return
          endif
          dex = find_closest(anytim(search_time),anytim(search.time.start))
          search=search[dex]
        end
        'MDI 96min Magnetogram': begin
          search=vso_search(min_time,max_time,inst='mdi',source='soho',/quiet,PHYSOBS='LOS_magnetic_field', provider='sdac')
          if datatype(search) ne 'STC' then begin
          if search eq '' then begin
            res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
            return
          endif
          endif
          searchindex=where(strmatch(search.fileid,'*96m*') eq 1)
          if n_elements(seachindex) eq 1 then begin
          if searchindex eq -1 then begin
          res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
          return
          endif & endif
          search=search[searchindex]
          dex = find_closest(anytim(search_time),anytim(search.time.start))
          search=search[dex]
        end
        'EUVI 195A': begin
          if val_source eq 'STA' then begin
            search=vso_search(min_time,max_time,wave='195',inst='secchi',source='stereo_a',/quiet)
            
            if datatype(search) ne 'STC' then begin
                res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
                return
              endif
              dex = find_closest(anytim(search_time),anytim(search.time.start))
              search=search[dex]
            endif
          
          if val_source eq 'STB' then begin
              search=vso_search(min_time,max_time,wave='195',inst='secchi',source='stereo_b',/quiet)
              if datatype(search) ne 'STC' then begin
                res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
                return
              endif
              dex = find_closest(anytim(search_time),anytim(search.time.start))
              search=search[dex]
            endif
        end
      endcase

      if datatype(search) ne 'STC' then begin
        res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
        return
      endif
     
      res=dialog_message( ['                             ',$
                           '       File found for:       ' ,$
                           '     '+search.time.start+'     ',$
                           '                             '], dialog_parent=ids.dl_main,/information)
     
      end
      
    'start':begin
      widget_control, /hourglass
      valy=WIDGET_info(ids.cbox_y, /combobox_gettext)
      valm=WIDGET_info(ids.cbox_m, /combobox_gettext)
      vald=WIDGET_info(ids.cbox_d, /combobox_gettext)
      valh=WIDGET_info(ids.cbox_h, /combobox_gettext)
      valmin=WIDGET_info(ids.cbox_min, /combobox_gettext)
      vals=WIDGET_info(ids.cbox_s, /combobox_gettext)
      search_time=strtrim(valy,2)+'-'+strtrim(valm,2)+'-'+strtrim(vald,2)+'T'+strtrim(valh,2)+':'+strtrim(valmin,2)+':'+strtrim(vals,2)
      search_time=anytim(anytim(search_time),/ccsds)
      min_time=anytim(anytim(search_time)-24.*3.*3600.,/ccsds)
      max_time=anytim(anytim(search_time)+24.*3.*3600.,/ccsds)

      val_source=WIDGET_info(ids.cbox_source, /combobox_gettext)
      vals_prod=WIDGET_info(ids.cbox_dprod, /combobox_gettext)
      case vals_prod of
        'AIA 193A': search=vso_search(near=search_time,start_time=min_time,end_time=max_time,wave='193',inst='aia',source='sdo',provider='JSOC',/quiet)
        '720s LOS HMI Magnetogram': search=vso_search(near=search_time,start_time=min_time,end_time=max_time,inst='hmi',source='sdo',PHYSOBS='vector_magnetic_field',provider='JSOC',/quiet)
        '45s LOS HMI Magnetogram':  search=vso_search(near=search_time,start_time=min_time,end_time=max_time,inst='hmi',source='sdo',PHYSOBS='LOS_magnetic_field',provider='JSOC',/quiet)
        'EIT 195A': begin
          search=vso_search(min_time,max_time,wave='195',inst='eit',source='soho',/quiet)
          if datatype(search) ne 'STC' then begin
            res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
            return
          endif
          dex = find_closest(anytim(search_time),anytim(search.time.start))
          search=search[dex]
        end
        'MDI 96min Magnetogram': begin
          search=vso_search(min_time,max_time,inst='mdi',source='soho',/quiet,PHYSOBS='LOS_magnetic_field', provider='sdac')
          if datatype(search) ne 'STC' then begin
          if search eq '' then begin
            res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
            return
          endif
          endif
          searchindex=where(strmatch(search.fileid,'*96m*') eq 1)
          if n_elements(seachindex) eq 1 then begin
          if searchindex eq -1 then begin
          res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
          return
          endif & endif
          search=search[searchindex]
          dex = find_closest(anytim(search_time),anytim(search.time.start))
          search=search[dex]
        end
        'EUVI 195A': begin
          if val_source eq 'STA' then begin
            search=vso_search(min_time,max_time,wave='195',inst='secchi',source='stereo_a',/quiet)
            
            if datatype(search) ne 'STC' then begin
                res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
                return
              endif
              dex = find_closest(anytim(search_time),anytim(search.time.start))
              search=search[dex]
            endif
          
          if val_source eq 'STB' then begin
              search=vso_search(min_time,max_time,wave='195',inst='secchi',source='stereo_b',/quiet)
              if datatype(search) ne 'STC' then begin
                res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
                return
              endif
              dex = find_closest(anytim(search_time),anytim(search.time.start))
              search=search[dex]
            endif
        end
      endcase

      if datatype(search) ne 'STC' then begin
        res=dialog_message( 'No File Found!', dialog_parent=ids.dl_main)
        return
      endif
      widget_control, ids.dl_path, get_value=temp_path
      dl=vso_get(search,out_dir=temp_path[0],/force,/quiet)
      res=dialog_message( 'File downloaded!', dialog_parent=ids.dl_main,/information)
      end
    else: dummy=0
  ENDCASE
  

END




;##########################################################################


