class ZCL_CA_MAIL_SMTP_HELPER definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_HELPER_MAIL_SMTP
*"* do not include other source files here!!!
  type-pools ABAP .

  types:
    begin of contacts_type,
        email(128) type c,
        cc(1)      type c,
      end of contacts_type .
  types:
    contacts_table_type type standard table of contacts_type with default key .
  types:
    object_text_table_type type standard table of solisti1 with default key .
  types:
    text_line_table_type type standard table of tline with default key .
  types:
    record_pack_table_type type standard table of sopcklsti1 with default key .
  types:
    receivers_table_type type standard table of somlreci1  with default key .
  types:
    binary_contents_table_type type standard table of solisti1 with default key .
  types:
    begin of binary_type,
        line(255),
      end of binary_type .
  types:
    binary_table_type type standard table of binary_type with default key .
  types EMAIL_ADDRESS_TYPE type SO_REC_EXT .

  class-methods CREATE
    importing
      !ID_SUBJECT type SO_OBJ_DES
      !IT_TEXT_LINES type TEXT_LINE_TABLE_TYPE optional
      !IT_BIN_LINES type SOLIX_TAB optional
    returning
      value(RO_INSTANCE) type ref to ZCL_CA_MAIL_SMTP_HELPER
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods ATTACH_DOCUMENT
    importing
      !IT_BIN type SOLIX_TAB
      !ID_NAME type SO_OBJ_DES
      !ID_DOCUMENT_TYPE type STRING
      !ID_LEN type I optional
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods ATTACH_FILE_FROM_BINARY
    importing
      !ID_CONTENTS type XSTRING
      !ID_DOCUMENT_TYPE type STRING
      !ID_NAME type SO_OBJ_DES
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods ATTACH_PDF_FROM_OTF
    importing
      !IT_OTF type TSFOTF
      !ID_NAME type SO_OBJ_DES
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods CONSTRUCTOR
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods GET_DOCUMENT_NUMBER
    returning
      value(RD_DOCNO) type SO_OBJ_NO .
  methods GET_DOCUMENT_TYPE
    returning
      value(RD_DOCTYPE) type SO_OBJ_TP .
  methods GET_DOCUMENT_YEAR
    returning
      value(RD_DOCYEAR) type SO_OBJ_YR .
  methods SEND_EMAIL
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods SEND_VIA_DIALOGUE
    importing
      !ID_SUBJECT type SOOD-OBJDES
      !IT_TEXT type BCSY_TEXT optional
      !ID_TEXT_FLOATING type OS_BOOLEAN optional
      !IT_RECIPIENTS type BCSY_RE3 optional
      !IO_SENDER type ref to IF_SENDER_BCS optional
      !IT_ATTACHMENTS type BCSY_IFDOC optional
      !IO_REPLYTO type ref to IF_RECIPIENT_BCS optional
      !IS_APPL_OBJECT type BORIDENT optional
      !ID_STARTING_AT_X type SY-TABIX optional
      !ID_STARTING_AT_Y type SY-TABIX optional
      !ID_ENDING_AT_Y type SY-TABIX optional
      !ID_ENDING_AT_X type SY-TABIX optional
      !ID_ENCRYPT type BCSD_ENCR optional
      !ID_SIGN type BCSD_SIGN optional
    returning
      value(RO_RESULT) type ref to CL_BCS
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods SETUP_MAIN_BODY_FROM_BIN
    importing
      !IT_LINES type SOLIX_TAB
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods SETUP_MAIN_BODY_FROM_TEXT
    importing
      !IT_LINES type TEXT_LINE_TABLE_TYPE
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods SET_EMAIL_SUBJECT
    importing
      !ID_SUBJECT type SO_OBJ_DES .
  methods SET_RECIPIENT_EMAIL_ADDRESS
    importing
      !ID_RECIPIENT_MAIL type ANY
      !ID_COPY type ABAP_BOOL optional .
  methods SET_SENDER_EMAIL_ADDRESS
    importing
      !ID_SENDER_MAIL type ANY
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
  methods SET_SENDER
    importing
      !IO_SENDER type ref to IF_SENDER_BCS
    raising
      ZCX_CA_MAIL_SMTP_HELPER .
protected section.

*"* protected components of class ZCL_HELPER_MAIL_SMTP
*"* do not include other source files here!!!
  data MT_RECEIVERS type RECEIVERS_TABLE_TYPE .
  data MS_DOCUMENT type SODOCCHGI1 .
  data MT_PACK type RECORD_PACK_TABLE_TYPE .
  data MT_OBJTEXT type OBJECT_TEXT_TABLE_TYPE .
  data MO_SEND_REQUEST type ref to CL_BCS .
  data MD_SUBJECT type SO_OBJ_DES .
  data MO_DOCUMENT type ref to CL_DOCUMENT_BCS .

  methods GET_BODY_RECORD_LINES
    returning
      value(RD_LINES) type I .
  private section.
ENDCLASS.



CLASS ZCL_CA_MAIL_SMTP_HELPER IMPLEMENTATION.


  method attach_document.

*Local declarations
    data: lx_bcs type ref to cx_bcs,
          ld_len type so_obj_len,
          ld_typ type so_obj_tp.
    data: ls_text_line type soli,
          lt_att_head  type soli_tab.

*Add the attachment to the document
    try.
        if id_len is not initial.
          ld_len = id_len.
        endif.

*Standard file type
        if strlen( id_document_type ) le 3.
          ld_typ = id_document_type.
          call method mo_document->add_attachment
            exporting
              i_attachment_type    = ld_typ
              i_attachment_subject = id_name
              i_att_content_hex    = it_bin
              i_attachment_size    = ld_len.

*...four+ char file type
        else.
          concatenate '&SO_FILENAME=' id_name '.' id_document_type into ls_text_line-line.
          append ls_text_line to lt_att_head.
          call method mo_document->add_attachment
            exporting
              i_attachment_type    = ld_typ
              i_attachment_subject = id_name
              i_att_content_hex    = it_bin
              i_attachment_size    = ld_len
              i_attachment_header  = lt_att_head.
        endif.

*Exceptions
      catch cx_document_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method attach_file_from_binary.

* local declarations
    data: lt_bin      type solix_tab.
    data: lx_bcs      type ref to cx_bcs.
    data: ld_len      type i.

* convert the binary string into a binary table
    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = id_contents
      importing
        output_length = ld_len
      tables
        binary_tab    = lt_bin.

* attach the document
    attach_document( it_bin           = lt_bin
                     id_document_type = id_document_type
                     id_name          = id_name
                     id_len           = ld_len ).

  endmethod.


  method attach_pdf_from_otf.

* local declarations
    data: ld_xbin   type xstring,
          ld_len_in type sood-objlen.

    data: lt_tline type standard table of tline,
          lt_bin   type solix_tab,
          ls_bin   like line of lt_bin,
          ls_pack  like line of mt_pack.

    data: lx_bcs      type ref to cx_bcs.

* convert the spool to PDF
    call function 'CONVERT_OTF'
      exporting
        format                = 'PDF'
        max_linewidth         = 255
      importing
        bin_filesize          = ld_len_in
        bin_file              = ld_xbin
      tables
        otf                   = it_otf
        lines                 = lt_tline
      exceptions
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        others                = 5.
    if sy-subrc <> 0.
      raise exception type zcx_ca_mail_smtp_helper
        exporting
          textid = zcx_ca_mail_smtp_helper=>conversion_otf_pdf.
    endif.

* convert the binary string into a binary table
    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer     = ld_xbin
      tables
        binary_tab = lt_bin.

* attach the document
    attach_document( it_bin           = lt_bin
                     id_document_type = 'PDF'
                     id_name          = id_name ).

  endmethod.


  method constructor.

* local declarations
    data: lx_bcs type ref to cx_send_req_bcs.

* create the persistence email object
    try.
        call method cl_bcs=>create_persistent
          receiving
            result = mo_send_request.
      catch cx_send_req_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method create.

*Setup the instance
    create object ro_instance.

*Setup the email subject
    ro_instance->set_email_subject( id_subject ).

*Setup the body ... bin or text
    if it_text_lines[] is not initial.
      ro_instance->setup_main_body_from_text( it_text_lines ).
    elseif it_bin_lines[] is not initial.
      ro_instance->setup_main_body_from_bin( it_bin_lines ).
    endif.

  endmethod.


  method get_body_record_lines.

    describe table mt_objtext lines rd_lines.

  endmethod.


  method get_document_number.

    rd_docno = mo_document->get_docno( ).

  endmethod.


  method get_document_type.

    rd_doctype = mo_document->get_doctp( ).

  endmethod.


  method get_document_year.

    rd_docyear = mo_document->get_docyr( ).

  endmethod.


  method send_email.

* local declarations
    data: ld_sent_to_all type c length 1,
          lx_bcs         type ref to cx_bcs.

* Send email
    try.
        call method mo_send_request->send
          exporting
            i_with_error_screen = 'X'
          receiving
            result              = ld_sent_to_all.
      catch cx_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method send_via_dialogue.

* local declarations
    data: lx_bcs type ref to cx_bcs.

    try.
        ro_result = mo_send_request->short_message( i_subject        = id_subject
                                                    i_text           = it_text
                                                    i_text_floating  = id_text_floating
                                                    i_recipients     = it_recipients
                                                    i_sender         = io_sender
                                                    i_attachments    = it_attachments
                                                    i_replyto        = io_replyto
                                                    i_appl_object    = is_appl_object
                                                    i_starting_at_x  = id_starting_at_x
                                                    i_starting_at_y  = id_starting_at_y
                                                    i_ending_at_y    = id_ending_at_y
                                                    i_ending_at_x    = id_ending_at_x
                                                    i_encrypt        = id_encrypt
                                                    i_sign           = id_sign ).
      catch cx_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method setup_main_body_from_bin.

* local declarations
    data: ls_line    like line of it_lines,
          ls_objtext like line of mt_objtext,
          ls_pack    like line of mt_pack,
          lx_bcs     type ref to cx_bcs.

* create the body of the document
    try.

        call method cl_document_bcs=>create_document
          exporting
            i_type    = 'HTM'
            i_subject = md_subject
            i_hex     = it_lines
          receiving
            result    = mo_document.

*   exceptions
      catch cx_document_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

* pass the document to send request
    try.

        call method mo_send_request->set_document
          exporting
            i_document = mo_document.

*   exceptions
      catch cx_send_req_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method setup_main_body_from_text.

* local declarations
    data: ls_line    like line of it_lines,
          ls_objtext like line of mt_objtext,
          ls_pack    like line of mt_pack,
          lx_bcs     type ref to cx_bcs.

    data: lt_message_body type bcsy_text.

* Process the text into the email body
    loop at it_lines into ls_line.
      append ls_line-tdline to lt_message_body.
    endloop.

* create the body of the document
    try.

        call method cl_document_bcs=>create_document
          exporting
            i_type    = 'RAW'
            i_subject = md_subject
            i_text    = lt_message_body
          receiving
            result    = mo_document.

*   exceptions
      catch cx_document_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

* pass the document to send request
    try.

        call method mo_send_request->set_document
          exporting
            i_document = mo_document.

*   exceptions
      catch cx_send_req_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method set_email_subject.

    md_subject = id_subject.

  endmethod.


  method set_recipient_email_address.

*Local declarations
    data: lo_recipient      type ref to if_recipient_bcs,
          ld_recipient_mail type adr6-smtp_addr.

*Create the recipient
    try.

        ld_recipient_mail = id_recipient_mail.
        call method cl_cam_address_bcs=>create_internet_address
          exporting
            i_address_string = ld_recipient_mail
          receiving
            result           = lo_recipient.

      catch cx_address_bcs.

    endtry.

*Set the recipient
    try.

        call method mo_send_request->add_recipient
          exporting
            i_recipient = lo_recipient
            i_express   = 'X'
            i_copy      = id_copy.

      catch cx_send_req_bcs.

    endtry.

  endmethod.


  method set_sender.

* local declarations
    data: lx_bcs          type ref to cx_bcs.

* create the sender instance and set it into the email
    try.
        call method mo_send_request->set_sender
          exporting
            i_sender = io_sender.
      catch cx_send_req_bcs cx_address_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.


  method set_sender_email_address.

* local declarations
    data: lo_sender      type ref to if_sender_bcs value is initial,
          ld_sender_mail type adr6-smtp_addr,
          lx_bcs         type ref to cx_bcs.

*Create the sender instance and set it into the email
    try.

        ld_sender_mail = id_sender_mail.
        set_sender( cl_cam_address_bcs=>create_internet_address( i_address_string = ld_sender_mail ) ).
      catch cx_send_req_bcs cx_address_bcs into lx_bcs.
        raise exception type zcx_ca_mail_smtp_helper
          exporting
            previous = lx_bcs.
    endtry.

  endmethod.
ENDCLASS.
