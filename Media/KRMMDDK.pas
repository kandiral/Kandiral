(******************************************************************************)
(*                                                                            *)
(*  Kandiral Ruslan                                                           *)
(*  https://kandiral.ru                                                       *)
(*                                                                            *)
(*  KRMMDDK                                                                   *)
(*  Ver.: 21.12.2016                                                          *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
unit KRMMDDK;

interface

// MMDDK.h

const
  DRV_LOAD              = $0001;
  DRV_ENABLE            = $0002;
  DRV_OPEN              = $0003;
  DRV_CLOSE             = $0004;
  DRV_DISABLE           = $0005;
  DRV_FREE              = $0006;
  DRV_CONFIGURE         = $0007;
  DRV_QUERYCONFIGURE    = $0008;
  DRV_INSTALL           = $0009;
  DRV_REMOVE            = $000A;

  DRV_RESERVED          = $0800;
  DRV_USER              = $4000;

  DRIVERS_SECTION = 'DRIVERS32'; // Section name for installed drivers
  MCI_SECTION     = 'MCI32';     // Section name for installed MCI drivers

  DCB_NOSWITCH   = $0008; // don't switch stacks for callback
  DCB_TYPEMASK   = $0007; // callback type mask
  DCB_NULL       = $0000; // unknown callback type

  // flags for wFlags parameter of DriverCallback()
  DCB_WINDOW     = $0001; // dwCallback is a HWND
  DCB_TASK       = $0002; // dwCallback is a HTASK
  DCB_FUNCTION   = $0003; // dwCallback is a FARPROC

  {BOOL APIENTRY DriverCallback(DWORD dwCallback, DWORD dwFlags,
      HDRVR hDevice, DWORD dwMsg, DWORD dwUser, DWORD dwParam1, DWORD dwParam2);}

  // generic prototype for audio device driver entry-point functions
  // midMessage(), modMessage(), widMessage(), wodMessage(), auxMessage()
  //typedef DWORD (SOUNDDEVMSGPROC)(WORD, WORD, DWORD, DWORD, DWORD);
  //typedef SOUNDDEVMSGPROC FAR *LPSOUNDDEVMSGPROC;

  DRVM_INIT              = 100;

  // message base for driver specific messages.
  //
  DRVM_MAPPER                     = $2000;
  DRVM_USER                       = $4000;
  DRVM_MAPPER_STATUS              = DRVM_MAPPER+0;
  DRVM_MAPPER_RECONFIGURE         = DRVM_MAPPER+1;
  DRVM_MAPPER_PREFERRED_GET       = DRVM_MAPPER+21;
  DRVM_MAPPER_PREFERRED_SET       = DRVM_MAPPER+22;
  DRVM_MAPPER_CONSOLEVOICECOM_GET = DRVM_MAPPER+23;
  DRVM_MAPPER_CONSOLEVOICECOM_SET = DRVM_MAPPER+24;

  // device ID for 386 AUTODMA VxD
  VADMAD_Device_ID    = $0444;

implementation

end.
