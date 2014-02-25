unit FMX.LateExecuter;

interface

uses
  System.Generics.Collections;

type
  TLateExecuter = class
  public type
    TExecFunc = reference to procedure;
    TExecStruct = record
      Func: TExecFunc;
      Wait: Integer;
      constructor Create(const iFunc: TExecFunc; const iWait: Integer);
    end;
  private var
    FFuncList: TList<TExecStruct>;
  private
    FEnabled: Boolean;
    FTerminated: Boolean;
    procedure Exec;
    function GetCount: Integer;
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Push(const iFunc: TExecFunc); overload;
    procedure Push(const iFunc: TExecFunc; const iWait: Integer); overload;
    procedure Remove(const iFunc: TExecFunc);
    procedure Clear;
    property Count: Integer read GetCount;
    property Terminated: Boolean read FTerminated;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

procedure LateExec(
  const iFunc: TLateExecuter.TExecFunc;
  const iWait: Integer = 0);

implementation

uses
  System.Classes;

var
  GLateExecuter: TLateExecuter = nil;

procedure LateExec(
  const iFunc: TLateExecuter.TExecFunc;
  const iWait: Integer = 0);
begin
  if (GLateExecuter = nil) then
    GLateExecuter := TLateExecuter.Create;

  GLateExecuter.Push(iFunc, iWait);
end;

{ TLateExecuter.TExecStruct }

constructor TLateExecuter.TExecStruct.Create(
  const iFunc: TExecFunc;
  const iWait: Integer);
begin
  Func := iFunc;
  Wait := iWait;
end;

{ TLateExecuter }

procedure TLateExecuter.Clear;
begin
  TMonitor.Enter(FFuncList);
  try
    FFuncList.Clear;
  finally
    TMonitor.Exit(FFuncList);
  end;
end;

constructor TLateExecuter.Create;
begin
  inherited Create;

  FFuncList := TList<TExecStruct>.Create;
  FEnabled := True;
  Exec;
end;

destructor TLateExecuter.Destroy;
begin
  Clear;

  FEnabled := False;

  while (not FTerminated) do
    TThread.Sleep(100);

  FFuncList.DisposeOf;

  inherited;
end;

procedure TLateExecuter.Exec;
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Struct: TExecStruct;
    begin
      FTerminated := False;

      while (FEnabled) do begin
        if (FFuncList.Count > 0) then begin
          TMonitor.Enter(FFuncList);
          try
            Struct := FFuncList[0];
          finally
            TMonitor.Exit(FFuncList);
          end;

          try
            if (Struct.Wait > 0) then
              TThread.Sleep(Struct.Wait);

            TThread.Synchronize(
              TThread.CurrentThread,
              procedure
              var
                Func: TExecFunc;
              begin
                Func := Struct.Func; // íºê⁄åƒÇ‘Ç∆ì‡ïîÉGÉâÅ[ C2183
                Func;
              end
            );
          finally
            TMonitor.Enter(FFuncList);
            try
              FFuncList.Delete(0);
            finally
              TMonitor.Exit(FFuncList);
            end;
          end;
        end
        else
          TThread.Sleep(100);
      end;

      Fterminated := True;
    end
  ).Start;
end;

function TLateExecuter.GetCount: Integer;
begin
  TMonitor.Enter(FFuncList);
  try
    Result := FFuncList.Count;
  finally
    TMonitor.Exit(FFuncList);
  end;
end;

procedure TLateExecuter.Push(const iFunc: TExecFunc; const iWait: Integer);
begin
  TMonitor.Enter(FFuncList);
  try
    FFuncList.Add(TExecStruct.Create(iFunc, iWait));
  finally
    TMonitor.Exit(FFuncList);
  end;
end;

procedure TLateExecuter.Push(const iFunc: TExecFunc);
begin
  Push(iFunc, 0);
end;

procedure TLateExecuter.Remove(const iFunc: TExecFunc);
var
  Struct: TExecStruct;
  i: Integer;
begin
  TMonitor.Enter(FFuncList);
  try
    for i := 0 to FFuncList.Count - 1 do begin
      Struct := FFuncList[i];

      if (@Struct.Func = @iFunc) then begin
        FFuncList.Delete(i);
        Break;
      end;
    end;
  finally
    TMonitor.Exit(FFuncList);
  end;
end;

procedure TLateExecuter.SetEnabled(const Value: Boolean);
begin
  if (FEnabled = Value) then
    Exit;

  FEnabled := Value;

  if (FEnabled) then
    Exec;
end;

initialization
finalization
begin
  GLateExecuter.Free;
end;

end.
