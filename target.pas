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


  { TTarget }

  TTarget = class(TObject)
  private
    FAddress: string;
    FID: integer;
    FLastLogEntry: TLogEntry;
  public
    property Address: string read FAddress write FAddress;
    property ID: integer read FID write FID;
    property LastLogEntry: TLogEntry read FLastLogEntry write FLastLogEntry;
    constructor Create;
    destructor Destroy; override;
  end;

  TTargetList = specialize TFPGObjectList<TTarget>;

implementation

{ TTarget }

constructor TTarget.Create;
begin
  inherited Create;
  FLastLogEntry := TLogEntry.Create;
end;

destructor TTarget.Destroy;
begin
  FreeAndNil(FLastLogEntry);
  inherited Destroy;
end;

end.
