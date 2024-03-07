unit target;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TLogEntry = class
    Time: TDateTime;
    Result: Boolean;
    PingTime: Integer;
    Interval: Cardinal;
  end;

  TLog = specialize TFPGObjectList<TLogEntry>;

  { TTarget }

  TTarget = class(TObject)
  private
    FAddress: String;
    FID: Integer;
    FLog: TLog;
  public
    property Address: String read FAddress write FAddress;
    property ID: Integer read FID write FID;
    property Log: TLog read FLog write FLog;
    constructor Create;
    destructor Destroy; override;
  end;

  TTargetList = specialize TFPGObjectList<TTarget>;

implementation

{ TTarget }

constructor TTarget.Create;
begin
  inherited Create;
  FLog:= TLog.Create;
end;

destructor TTarget.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

end.

