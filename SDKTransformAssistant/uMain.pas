unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects, FMX.Edit, FMX.ScrollBox,
  FMX.Memo, FMX.ListBox;

type
  TfrmMain = class(TForm)
    grpbxIOS: TGroupBox;
    rdoIOS: TRadioButton;
    stylebookMain: TStyleBook;
    lblIOSRoot: TLabel;
    rectTop: TRectangle;
    textTop: TText;
    grpbxOSX: TGroupBox;
    rdoOSX: TRadioButton;
    lblOSXRoot: TLabel;
    layoutClang: TLayout;
    layoutClangPath: TLayout;
    edtClang: TEdit;
    btnClangRef: TButton;
    lblClang: TLabel;
    lineSep1: TLine;
    btnExec: TButton;
    layoutSDKTransPath: TLayout;
    layoutSDKTransPath2: TLayout;
    edtSDKTransPath: TEdit;
    btnSDKTransPathRef: TButton;
    Label1: TLabel;
    memoLog: TMemo;
    cmbbxIOSSDKs: TComboBox;
    cmbbxOSXSDKs: TComboBox;
    layoutOpBase: TLayout;
    rectWaiter: TRectangle;
    aniWaiter: TAniIndicator;
    lineSep2: TLine;
    layoutOutput: TLayout;
    layoutOutput2: TLayout;
    edtOutputPath: TEdit;
    btnOutputRef: TButton;
    lblOutputPath: TLabel;
    layoutLogOp: TLayout;
    btnClearLog: TButton;
    btnCopyLog: TButton;
    layoutLogOp2: TLayout;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
    procedure btnSDKTransPathRefClick(Sender: TObject);
    procedure btnClangRefClick(Sender: TObject);
    procedure btnOutputRefClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnCopyLogClick(Sender: TObject);
    procedure edtSDKTransPathChange(Sender: TObject);
    procedure edtClangChange(Sender: TObject);
  private type
    TSDKData = record
      FName, FPath: String;
      constructor Create(const iName, iPath: String);
    end;
    TSDKList = TList<TSDKData>;
  private var
    FBDSPath: String;
    FBinPath: String;
    FPlatformSdkPath: String;
    FSDKTransformExe: String;
    FClang: String;
    FIOSSDKs: TSDKList;
    FOSXSDKs: TSDKList;
  private
    function ReadReg(const iKey: String): String;

    procedure AddLog(const iMsg: String);
    procedure AddError(const iMsg: String);

    procedure FindSDKs(
      const iPrefix: String;
      const iList: TSDKList;
      const iComboBox: TComboBox);

    procedure SelectDir(const iTitle: String; const iEdit: TEdit);
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils,
  System.IniFiles,
  System.Generics.Defaults,
  Winapi.Windows,
  FMX.Platform,
  uGetEnvironmentVariables,
  uExecCMD,
  uRegistoryUtils,
  uCautionForm
  ;

const
  CCOPTS_IOS =
    '-D TARGET_OS_IPHONE ' +
    '-isysroot "%s" ' + // SDKRoot
    '-isystem "%s" ' +  // Include
    '-isystem "%s" ' +  // Clang
    '-F "%s" ' +        // Framework
    '-triple thumbv7-apple-ios';

  CCOPTS_OSX =
    '--macsdk ' +
    '-D TARGET_OS_MAC ' +
    '-isysroot "%s" ' + // SDKRoot
    '-isystem "%s" ' +  // Include
    '-isystem "%s" ' +  // Clang
    '-F "%s" ' +        // Framework
    '-triple x86_64-apple-macosx-clang++';

  OPTS =
    '-cc1 ' +
    '-g ' +
    '-w ' +
    '%s ' +
    '-fdiagnostics-show-option ' +
    '-fexceptions ' +
    '-fobjc-exceptions ' +
    '-x objective-c ' +
    '-std=gnu99 ' +
    '-nobuiltininc ' +
    '-nostdinc++ ' +
    '-nostdsysteminc ' +
    '-fblocks';

  INCLUDE_PATH = '%s\usr\include';
  FRAMEWORK_PATH =  '%s\System\Library\Frameworks';

  CMD = 'SdkTransform.exe %s --fmt %s';

  ENV_BDS = 'BDS';
  ENV_BDSBIN = 'BDSBIN';
  ENV_BDSPLATFORMSDKSDIR = 'BDSPLATFORMSDKSDIR';

  SDK_TRANSFORM_EXE = 'SdkTransform.exe';
  SDK_TRANSFORM_TYPES = 'SdkTransformTypes.txt';

  SDK_PREFIX_IOS = 'iPhoneOS';
  SDK_PREFIX_OSX = 'MacOSX';

  SDKS_PATH = 'PlatformSDKs';
  NDK_PATH = 'android-ndk-r9c';

  CLANG_PATH = 'toolchains\llvm-3.3\prebuilt\windows\lib\clang\3.3\include';

  // for GetIt
  NDK_PATTERN = 'AndroidNDK*';
  NDK_PATTERN2 = 'android-ndk*';
  CLANG_PATH2 =
    'Embarcadero\Studio\18.0\CatalogRepository\' +
    'AndroidNDK-9c_x86_GIB.Build.22858.6822\' +
    CLANG_PATH;

  BDS_REG_PATH = 'Software\Embarcadero\BDS\18.0\';
  BDS_REG_KEY_ROOT_DIR = 'RootDir';
  BDS_REG_KEY_APP = 'App';
  BDS_REF_KEY_ANDROID_PATH = 'AndroidPath';

  IMPORTED_SDKS_PATH = 'Embarcadero\Studio\SDKs';

{$R *.fmx}

{ TfrmMain.TSDKData }

constructor TfrmMain.TSDKData.Create(const iName, iPath: String);
begin
  FName := iName;
  FPath := iPath;
end;

procedure TfrmMain.AddError(const iMsg: String);
begin
  AddLog('ERROR: ' + iMsg);
end;

procedure TfrmMain.AddLog(const iMsg: String);
begin
  memoLog.Lines.Add(iMsg);
  memoLog.ScrollBy(0, MaxInt);
end;

procedure TfrmMain.btnClangRefClick(Sender: TObject);
begin
  SelectDir('Clang Path', edtClang);
end;

procedure TfrmMain.btnClearLogClick(Sender: TObject);
begin
  memoLog.Lines.Clear;
  memoLog.Lines.Add(''); // Scroll bar を消すため
  memoLog.Lines.Clear;
end;

procedure TfrmMain.btnCopyLogClick(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
begin
  if
    TPlatformServices.Current.SupportsPlatformService(
      IFMXClipboardService,
      IInterface(Clipboard))
  then
    Clipboard.SetClipboard(memoLog.Lines.Text);
end;

procedure TfrmMain.btnExecClick(Sender: TObject);
var
  Opt: String;
  Root: String;
  Include: String;
  Framework: String;

  function CheckFile(const iMsg, iPath: String): Boolean;
  begin
    Result := TDirectory.Exists(iPath) or TFile.Exists(iPath);
    if (Result) then
      AddLog(iMsg + ': ' + iPath)
    else
      AddError(iMsg + ' is not found.');
  end;

begin
  if (not CheckFile('Output', edtOutputPath.Text)) then
  begin
    ShowMessage('Please input output path.');
    Exit;
  end;

  // Transrate
  try
    rectWaiter.Visible := True;
    aniWaiter.Enabled := True;

    AddLog('----- Start');

    if (rdoIOS.IsChecked) then
    begin
      // IOS
      Root := FIOSSDKs[cmbbxIOSSDKs.ItemIndex].FPath;
      Opt := CCOPTS_IOS
    end
    else
    begin
      // OSX
      Root := FOSXSDKs[cmbbxOSXSDKs.ItemIndex].FPath;
      Opt := CCOPTS_OSX;
    end;

    Include := Format(INCLUDE_PATH, [Root]);
    Framework := Format(FRAMEWORK_PATH, [Root]);

    if (not CheckFile('BDS\Bin', FBinPath)) then
      Exit;

    if (not CheckFile('SDK Root', Root)) then
      Exit;

    if (not CheckFile('SDK Include', Include)) then
      Exit;

    if (not CheckFile('SDK Framework', Framework)) then
      Exit;

    if (not CheckFile('Clang', FClang)) then
      Exit;

    if (not CheckFile(SDK_TRANSFORM_EXE, FSDKTransformExe)) then
      Exit;

    if
      not CheckFile(
        SDK_TRANSFORM_TYPES,
        TPath.Combine(FBinPath, SDK_TRANSFORM_TYPES)
      )
    then
      Exit;

    Opt := Format(Opt, [Root, Include, FClang, Framework]);
    Opt := Format(OPTS, [Opt]);

    TDirectory.SetCurrentDirectory(FBinPath);

    ExecCMD(
      FSDKTransformExe + ' ' + Opt + ' --fmt --out:' + edtOutputPath.Text,
      procedure(const iLine: String)
      begin
        AddLog(iLine);
        Application.ProcessMessages;
      end
    );

    AddLog('----- Finish');
  finally
    aniWaiter.Enabled := False;
    rectWaiter.Visible := False;
  end;
end;

procedure TfrmMain.btnOutputRefClick(Sender: TObject);
begin
  SelectDir('Output', edtOutputPath);
end;

procedure TfrmMain.btnSDKTransPathRefClick(Sender: TObject);
begin
  if (TDirectory.Exists(edtSDKTransPath.Text)) then
    dlgOpen.InitialDir := TPath.GetDirectoryName(edtSDKTransPath.Text);

  if (dlgOpen.Execute) then
    edtSDKTransPath.Text := dlgOpen.FileName;
end;

procedure TfrmMain.edtClangChange(Sender: TObject);
begin
  FClang := edtClang.Text;
end;

procedure TfrmMain.edtSDKTransPathChange(Sender: TObject);
begin
  FSDKTransformExe := edtSDKTransPath.Text;
end;

procedure TfrmMain.FindSDKs(
  const iPrefix: String;
  const iList: TSDKList;
  const iComboBox: TComboBox);
var
  Paths: TStringDynArray;
  Path: String;
  i: Integer;
begin
  iList.Clear;
  iComboBox.Items.Clear;

  Paths := TDirectory.GetDirectories(FPlatformSdkPath, iPrefix + '*');
  for Path in Paths do
    iList.Add(TSDKData.Create( TPath.GetFileName(Path), Path));

  iList.Sort(
    TComparer<TSDKData>.Construct(
      function(const iL, iR: TSDKData): Integer
      var
        Res: Double;
      begin
        Res :=
          TDirectory.GetCreationTime(iR.FPath)
          - TDirectory.GetCreationTime(iL.FPath);

        Result := Ord(Res > 0) - Ord(Res < 0);
      end
    )
  );

  for i := 0 to iList.Count - 1 do
    iComboBox.Items.Add(iList[i].FName);

  if (iComboBox.Count > 0) then
    iComboBox.ItemIndex := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  SDKFound: Boolean;
  RegValue: String;
  ClangPath: String;

  procedure FindClangPath(const iDirs: array of String);
  var
    Dir, Path: String;
    OK: Boolean;
  begin
    OK := True;
    Path := '';

    for Dir in iDirs do
    begin
      Path := TPath.Combine(Path, Dir);

      if (not TDirectory.Exists(Path)) then
      begin
        OK := False;
        Break;
      end;
    end;

    if (OK) then
      FClang := Path;
  end;

  function FindClangPath2(const iPattern: String): Boolean;
  var
    Dirs: TStringDynArray;
    Dir: String;
  begin
    Result := False;

    Dirs := TDirectory.GetDirectories(RegValue, iPattern);
    for Dir in Dirs do
    begin
      ClangPath := TPath.Combine(Dir, CLANG_PATH);
      Result := TDirectory.Exists(ClangPath);
      if (Result) then
      begin
        FClang := ClangPath;
        Break;
      end;
    end;
  end;

  procedure CheckPath(const iTitle: String; var ioPath: String);
  begin
    if (not TDirectory.Exists(ioPath)) and (not TFile.Exists(ioPath)) then
    begin
      ioPath := '';
      AddLog(iTitle + ' is not found.');
    end;
  end;

begin
  rectWaiter.Visible := False;

  FIOSSDKs := TSDKList.Create;
  FOSXSDKs := TSDKList.Create;

  // Get BDS Path
  GetEnvironmentVariables(
    procedure(const iKey, iValue: String)
    begin
      if (ENV_BDS = iKey) then
        FBDSPath := iValue;

      if (ENV_BDSBIN = iKey) then
        FBinPath := iValue;

      if (ENV_BDSPLATFORMSDKSDIR = iKey) then
        FPlatformSdkPath := iValue;
    end
  );

  if (FBDSPath.IsEmpty) then
  begin
    RegValue := ReadReg(BDS_REG_KEY_ROOT_DIR);

    if (not RegValue.IsEmpty) then
      FBDSPath := RegValue;
  end;

  if (FBinPath.IsEmpty) then
  begin
    RegValue := ReadReg(BDS_REG_KEY_APP);
    if (TFile.Exists(RegValue)) then
      FBinPath := TPath.GetDirectoryName(RegValue);
  end;

  if (FPlatformSdkPath.IsEmpty) then
  begin
    RegValue := TPath.Combine(TPath.GetDocumentsPath, IMPORTED_SDKS_PATH);
    if (TDirectory.Exists(RegValue)) then
      FPlatformSdkPath := RegValue;
  end;

  // Path Check
  CheckPath('BDS', FBDSPath);
  CheckPath('BDS\Bin', FBinPath);
  CheckPath('PlatformSDK', FPlatformSdkPath);

  // Find Clang Path
  // Find Clang Path: Check Installed by ISO
  FindClangPath([FBDSPath, SDKS_PATH, NDK_PATH, CLANG_PATH]);

  // Find Clang Path: Check Installed by GetIt
  if (FClang.IsEmpty) then
  begin
    ClangPath := TPath.Combine(TPath.GetSharedDocumentsPath, CLANG_PATH2);
    if (TDirectory.Exists(ClangPath)) then
      FClang := ClangPath;
  end;

  // Find Clang Path: from Registory
  if (FClang.IsEmpty) then
  begin
    RegValue := ReadReg(BDS_REF_KEY_ANDROID_PATH);

    if (TDirectory.Exists(RegValue)) then
    begin
      RegValue := TPath.GetDirectoryName(RegValue);
      if (TDirectory.Exists(RegValue)) then
      begin
        if (not FindClangPath2(NDK_PATTERN)) then
          FindClangPath2(NDK_PATTERN2);
      end;
    end;
  end;

  CheckPath('NDK', FClang);
  edtClang.Text := FClang;

  // Find SDKTransform
  FSDKTransformExe := TPath.Combine(FBinPath, SDK_TRANSFORM_EXE);
  CheckPath(SDK_TRANSFORM_EXE, FSDKTransformExe);
  edtSDKTransPath.Text := FSDKTransformExe;

  // Find SDK
  SDKFound := False;
  if (TDirectory.Exists(FPlatformSdkPath)) then
  begin
    FindSDKs(SDK_PREFIX_IOS, FIOSSDKs, cmbbxIOSSDKs);
    FindSDKs(SDK_PREFIX_OSX, FOSXSDKs, cmbbxOSXSDKs);

    SDKFound := (FIOSSDKs.Count + FOSXSDKs.Count > 0);
  end;

  if (not SDKFound) then
  begin
    cmbbxIOSSDKs.Enabled := False;
    cmbbxOSXSDKs.Enabled := False;
    btnExec.Enabled := False;

    TfrmCaution.ShowSelf;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FIOSSDKs.DisposeOf;
  FOSXSDKs.DisposeOf;
end;

function TfrmMain.ReadReg(const iKey: String): String;
begin
  Result := GetRegValue2(HKEY_CURRENT_USER, BDS_REG_PATH + iKey);
end;

procedure TfrmMain.SelectDir(const iTitle: String; const iEdit: TEdit);
var
  Dir: String;
begin
  if (SelectDirectory(iTitle, '', Dir)) then
    iEdit.Text := Dir;
end;

end.
