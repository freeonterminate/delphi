unit FMX.SystemFontService;

interface

uses
  FMX.Platform;

type
  // TInterfacedObject と目的のサービスのインターフェースを継承する
  TFMXSystemFontServiceHook = class(TInterfacedObject, IFMXSystemFontService)
  private var
    // 元々 FMX が登録していたサービスを保持する変数
    FOrgFMXSystemFontService: IFMXSystemFontService;
  protected
    // コンストラクタの中でサービスを置き換える
    constructor Create;
  public
    // 置き換えたいサービスが実装すべきメソッド
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: String;
    function GetDefaultFontSize: Single; inline; // inline ついてはメソッド内に
  public
    // これを呼ぶとコンストラクタを呼べる！
    // 今回は Initialization で呼ぶ
    class procedure RegistService;
  end;

implementation

var
  // このサービスのインスタンスを保持する
  SystemFontServiceHook: TFMXSystemFontServiceHook = nil;

{ TFMXSystemFontServiceHook }

constructor TFMXSystemFontServiceHook.Create;
begin
  inherited;

  // オリジナルの Interface を取り出して自分自身と置き換えてしまう！
  if
    TPlatformServices.Current.SupportsPlatformService( // 取り出し
      IFMXSystemFontService,
      IInterface(FOrgFMXSystemFontService))
  then begin
    // オリジナルを削除
    TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
    // 自分を登録しちゃう
    TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, Self);
  end;
end;

function TFMXSystemFontServiceHook.GetDefaultFontFamilyName: String;
begin
  // 元々のサービスが返す値を変えちゃう！
  Result := 'メイリオ';
end;

function TFMXSystemFontServiceHook.GetDefaultFontSize: Single;
begin
  // 元々のサービスの値を返すこともできる
  // 置き換える予定の無いメソッドについては inline 指定をすると
  // 呼び出しオーバーヘッドが少なくなる
  Result := FOrgFMXSystemFontService.GetDefaultFontSize;
end;

// Initialization で↓このメソッドを呼んでコンストラクタを呼び出す
// class constractor でも可能
// これによって、このユニットをプロジェクトを追加するだけで自動的に置き換わる！
class procedure TFMXSystemFontServiceHook.RegistService;
begin
  if (SystemFontServiceHook = nil) then
    SystemFontServiceHook := TFMXSystemFontServiceHook.Create;
end;

initialization
  TFMXSystemFontServiceHook.RegistService;

end.
