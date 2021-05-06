class ZCX_CA_MAIL_SMTP_HELPER definition
  public
  inheriting from ZCX_CA_ROOT
  create public .

public section.

  constants:
    begin of CONVERSION_OTF_PDF,
      msgid type symsgid value 'ZCA_MAIL_SMTP',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CONVERSION_OTF_PDF .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MS_BALMSG type BAL_S_MSG optional
      !MT_BALMSG type BAL_TT_MSG optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_MAIL_SMTP_HELPER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MS_BALMSG = MS_BALMSG
MT_BALMSG = MT_BALMSG
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
