unit uDropSource;

interface

uses
  System.Classes, Vcl.Controls;

type
  TOnGetStreamEvent =
    procedure (out oStream: TStream; out oFileName: String) of object;

procedure SetDropSource(
  const iWinControl: TWinControl;
  const iOnGetStreamEvent: TOnGetStreamEvent);

procedure UnsetDropSource(const iWinControl: TWinControl);

implementation

uses
  System.Types, Winapi.Windows, Winapi.Messages, System.Generics.Collections,
  Vcl.Forms, Winapi.ShlObj, Winapi.ActiveX, uUtils, uShellLink, uDebug,
  System.SysUtils;

type
  TDropSourceControl = class(TComponent, IDropSource)
  private
    FControl: TWinControl;
    FOrgWndProc: TWndMethod;
    FOnGetStreamEvent: TOnGetStreamEvent;
    FIsLButtonDown: Boolean;
    FIsDragging: Boolean;
    FDragStartPos: TPoint;
    procedure WndProc(var iMsg: TMessage);
    destructor Destroy; override;
    procedure Cancel;

    function QueryContinueDrag(
      fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  end;

  (*
  TEnumFormatEtc class(TInterfacedObject, IEnumFormatEtc)
  public
    function Next(
      celt: ULONG;
      const rgelt: TFormatEtc ;
      const pceltFetched: ULONG ): HResult; stdcall;
    {
    virtual HRESULT __stdcall Skip(ULONG celt);
    virtual HRESULT __stdcall Reset(void);
    virtual HRESULT __stdcall Clone(IEnumFORMATETC ** ppenum);
    }
  end;
  *)

  TDataObject = class(TComponent, IDataObject, IAsyncOperation)
  private
    FFileName: String;
    FDropSourceControl: TDropSourceControl;
  public
    constructor Create(
      const iDropSourceControl: TDropSourceControl); reintroduce;
    { IDataObject }
    function GetData(
      const formatetcIn: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function GetDataHere(
      const formatetc: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(
      const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(
      const formatetc: TFormatEtc;
      var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(
      dwDirection: Longint;
      out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(
      const formatetc: TFormatEtc;
      advf: Longint;
      const advSink: IAdviseSink;
      out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

var
  GControls: TList<TDropSourceControl>;
  CF_FILECONTENTS: UINT;

{ TDropSourceControl }

procedure TDropSourceControl.Cancel;
begin
  FIsLButtonDown := False;
  FIsDragging := False;
end;

destructor TDropSourceControl.Destroy;
begin
  if (FControl <> nil) then
    FControl.WindowProc := FOrgWndProc;

  inherited;
end;

function TDropSourceControl.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TDropSourceControl.QueryContinueDrag(
  fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  Result := S_OK;

  if (fEscapePressed) then
    Result := DRAGDROP_S_CANCEL;

  if ((grfKeyState and (MK_LBUTTON or MK_RBUTTON)) = 0) then
    Result := DRAGDROP_S_DROP;
end;

procedure TDropSourceControl.WndProc(var iMsg: TMessage);
var
  Pos: TPoint;
  Effect: LongInt;
  DataObject: TDataObject;
begin
  Self.FOrgWndProc(iMsg);

  case iMsg.Msg of
    WM_DESTROY: begin
      FControl := nil;
    end;

    WM_LBUTTONDOWN: begin
      FIsLButtonDown := True;
      FDragStartPos := Mouse.CursorPos;
    end;

    WM_KEYDOWN: begin
      if (TWMKeyDown(iMsg).CharCode = VK_ESCAPE) then
        Cancel;
    end;

    WM_LBUTTONUP, WM_RBUTTONDOWN, WM_MBUTTONDOWN: begin
      Cancel;
    end;

    WM_MOUSEMOVE: begin
      if (FIsLButtonDown) then begin
        Pos := Mouse.CursorPos;
        Pos := Pos - FDragStartPos;

        if
          (Abs(Pos.X) > Mouse.DragThreshold) or
          (Abs(Pos.Y) > Mouse.DragThreshold)
        then begin
          FIsDragging := True;

          Effect := DROPEFFECT_NONE;

          DataObject := TDataObject.Create(Self);
          try
            DoDragDrop(DataObject, Self, DROPEFFECT_COPY, Effect);
            Cancel;
          finally
            DataObject.Free;
          end;
        end;
      end;
    end;
  end;
end;

{ TDataObject }

constructor TDataObject.Create(const iDropSourceControl: TDropSourceControl);
begin
  inherited Create(nil);

  FDropSourceControl := iDropSourceControl;
end;

function TDataObject.DAdvise(
  const formatetc: TFormatEtc;
  advf: Integer;
  const advSink: IAdviseSink;
  out dwConnection: Integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.DUnadvise(dwConnection: Integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumFormatEtc(
  dwDirection: Integer;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  if (dwDirection = DATADIR_GET) then
    Result := S_OK
  else
    Result := E_NOTIMPL;
end;

function TDataObject.GetCanonicalFormatEtc(
  const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TDataObject.GetData(
  const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
const
  DUMMY_FILE: String = 'DUMMY_FILE.bin';
var
  Global: HGLOBAL;
  Data: Pointer;
begin
  ZeroMemory(@medium, SizeOf(medium));

  Result := DV_E_FORMATETC;

  if (QueryGetData(formatetcIn) = S_OK) then begin
    case formatetcIn.cfFormat of
      CF_HDROP: begin
        Trace('GetData');

        //Stream := nil;
        //FDropSourceControl.FOnGetStreamEvent(Stream, FFileName);

        Global := GlobalAlloc(0, Length(DUMMY_FILE));
        Data := GlobalLock(Global);
        try
          Move(PChar(DUMMY_FILE)^, Data^, GlobalSize(Global));
        finally
          GlobalUnlock(Global);
        end;

        medium.hGlobal := Global;
        medium.tymed := TYMED_HGLOBAL;
        medium.lpszFileName := Data;
        {
        if (Stream = nil) then
          Result := E_FAIL
        else begin
          medium.tymed := TYMED_FILE;
          //medium.stm := TStreamAdapter.Create(Stream);
          medium.lpszFileName := PChar(FFileName);
        end;
        }
      end;
    end;
  end;
end;

function TDataObject.GetDataHere(
  const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
end;

function TDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  Result := DV_E_FORMATETC;

  if
    (formatetc.cfFormat = CF_FILECONTENTS) and
    (formatetc.dwAspect and DVASPECT_CONTENT <> 0) and
    (formatetc.tymed and TYMED_HGLOBAL <> 0)
  then
    Result := S_OK;
end;

function TDataObject.SetData(
  const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

{ Global }

procedure SetDropSource(
  const iWinControl: TWinControl;
  const iOnGetStreamEvent: TOnGetStreamEvent);
var
  DSC: TDropSourceControl;
begin
  DSC := TDropSourceControl.Create(nil);

  DSC.FControl := iWinControl;
  DSC.FOrgWndProc := iWinControl.WindowProc;
  DSC.FOnGetStreamEvent := iOnGetStreamEvent;

  iWinControl.WindowProc := DSC.WndProc;
end;

procedure UnsetDropSource(const iWinControl: TWinControl);
var
  DSC: TDropSourceControl;
begin
  for DSC in GControls do begin
    if (DSC.FControl = iWinControl) then begin
      GControls.Remove(DSC);
      DSC.Free;

      Break;
    end;
  end;
end;

procedure ClearDropSource;
var
  DSC: TDropSourceControl;
begin
  for DSC in GControls do
    DSC.Free;

  GControls.Clear;
end;

initialization
begin
  OleInitialize(nil);

  GControls := TList<TDropSourceControl>.Create
  CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
end;

finalization
begin
  OleUninitialize;

  ClearDropSource;
  GControls.Free;
end;

end.
