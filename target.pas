unit target;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TLogEntry = class
    Start: TDateTime;
    Result: boolean;
    PingTime: integer;
    Interval: cardinal;
  end;

  TLog = specialize TFPGObjectList<TLogEntry>;

  { TTarget }

  TTarget = class(TObject)
  private
    FAddress: string;
    FID: integer;
    FLog: TLog;
  public
    property Address: string read FAddress write FAddress;
    property ID: integer read FID write FID;
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
  FLog := TLog.Create;
end;

destructor TTarget.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

end.
