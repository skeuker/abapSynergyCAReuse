class zcl_ca_file_utils definition
  public
  final
  create public .

  public section.
    class-methods: get_dir_listing
      importing id_dirname  type epsdirnam
                id_filemask type epsfilnam optional
      exporting et_dirlist  type zca_tt_dir_list
      raising   zcx_ca_root.
    constants gc_trans_dir type epssubdir value '$TR_'.
  protected section.
  private section.

ENDCLASS.



CLASS ZCL_CA_FILE_UTILS IMPLEMENTATION.


  method get_dir_listing.

*Local data declaration
    data: ls_balmsg        type bal_s_msg,
          ld_file_counter  type epsfilsiz,
          ld_error_counter type epsfilsiz,
          ls_dirlist       type zca_s_dir_list,
          lt_dirlist       type zca_tt_dir_list,
          begin of ls_file,
            dirname(75) type c, " name of directory
            name(75)    type c, " name of entry
            type(10)    type c, " type of entry
            len(8)      type p, " length in bytes
            owner(8)    type c, " owner of the entry
            mtime(6)    type p, " last modification date, seconds since 1970
            mode(9)     type c, " like "rwx-r-x--x": protection mode
            errno(3)    type c,
            errmsg(40)  type c,
          end of ls_file.

*On request to view transport directory content
    if id_dirname(4) = gc_trans_dir. "Unintended use
      message e001(zca_file) into data(ld_message).
      ls_balmsg = corresponding #( syst ).
      raise exception type zcx_ca_root
        exporting
          ms_balmsg = ls_balmsg.
    endif.

*Close out any prior read on operating system level
    call 'C_DIR_READ_FINISH' "#EC CI_CCALL
          id 'ERRNO'  field ls_file-errno
          id 'ERRMSG' field ls_file-errmsg.

*Note that to use the id_filemask parameter:
* only works with * in last position

*Start reading the requested file directory
    call 'C_DIR_READ_START' "#EC CI_CCALL
          id 'DIR'    field id_dirname
          id 'FILE'   field id_filemask
          id 'ERRNO'  field ls_file-errno
          id 'ERRMSG' field ls_file-errmsg.

*...exception handling: failed to read directory content
    if sy-subrc <> 0.
      message e002(zca_file)
        with id_dirname into ld_message.
      ls_balmsg = corresponding #( syst ).
      raise exception type zcx_ca_root
        exporting
          ms_balmsg = ls_balmsg.
    endif.

*Read file directory while there is more content
    do.

*Prepare for next read
      clear: ls_file, ls_dirlist.

*Read next entry from the file directory
      call 'C_DIR_READ_NEXT' "#EC CI_CCALL
            id 'TYPE'   field ls_file-type
            id 'NAME'   field ls_file-name
            id 'LEN'    field ls_file-len
            id 'OWNER'  field ls_file-owner
            id 'MTIME'  field ls_file-mtime
            id 'MODE'   field ls_file-mode
            id 'ERRNO'  field ls_file-errno
            id 'ERRMSG' field ls_file-errmsg.

*Processing by directory read outcome
      case sy-subrc.

*File read successful: keep track of result
        when 0.

*...format file size for feedback to caller
          if ls_file-len > 2147483647.
            ls_dirlist-size  = -99.
          else.
            ls_dirlist-size  = ls_file-len.
          endif.

*...keep track of directory entry read
          ls_dirlist-name = ls_file-name.
          if ls_file-type(1) = 'f' or              " regular file
             ls_file-type(1) = 'F'.
            ls_dirlist-rc   = 0.
            append ls_dirlist to lt_dirlist.
            add 1 to ld_file_counter.
          endif.

*No further file directory content: stop reading
        when 1.
          exit.

*File read failed: keep track of error count
        when others.

*...keep track of error count
          add 1 to ld_error_counter.

*...on encountering a (arbitrary) number of read failures
          if ld_error_counter > 1000.

*....stop reading file directory content on OS level
            call 'C_DIR_READ_FINISH' "#EC CI_CCALL
                  id 'ERRNO'  field ls_file-errno
                  id 'ERRMSG' field ls_file-errmsg.

*....exception handling: too many errors on read
            message e003(zca_file)
              with id_dirname into ld_message.
            ls_balmsg = corresponding #( syst ).
            raise exception type zcx_ca_root
              exporting
                ms_balmsg = ls_balmsg.

          endif.

      endcase.

    enddo.

*Close out file directory read on operating system
    call 'C_DIR_READ_FINISH' "#EC CI_CCALL
          id 'ERRNO'  field ls_file-errno
          id 'ERRMSG' field ls_file-errmsg.

*Feedback to caller
    et_dirlist = lt_dirlist.

  endmethod.
ENDCLASS.
