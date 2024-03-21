unit target;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TLogEntry = class
    Start: TDateTime;
    Successful: boolean;
    PingTime: integer;
  end;

  { TTarget }

  TTarget = class(TObject)
  private
    FActive: Boolean;
    FAddress: String;
    FID: Integer;
    FPosition: Integer;
    FCurrentLogEntry: TLogEntry;
    FNumberOfErrors: Integer;
    FRunning: Boolean;
    FCurrentErrors: Integer;
    FWarningShown: Boolean;
  public
    property Active: Boolean read FActive write FActive;
    property Address: String read FAddress write FAddress;
    property ID: Integer read FID write FID;
    property Position: Integer read FPosition write FPosition;
    property CurrentLogEntry: TLogEntry read FCurrentLogEntry write FCurrentLogEntry;
    property NumberOfErrors: Integer read FNumberOfErrors write FNumberOfErrors;
    property Running: Boolean read FRunning write FRunning;
    property CurrentErrors: Integer read FCurrentErrors write FCurrentErrors;
    property WarningShown: Boolean read FWarningShown write FWarningShown;
    constructor Create;
    destructor Destroy; override;
  end;

  TTargetList = specialize TFPGObjectList<TTarget>;

implementation

{ TTarget }

constructor TTarget.Create;
begin
  inherited Create;
  FActive:= True;
  FCurrentLogEntry := TLogEntry.Create;
end;

destructor TTarget.Destroy;
begin
  FreeAndNil(FCurrentLogEntry);
  inherited Destroy;
end;

end.
