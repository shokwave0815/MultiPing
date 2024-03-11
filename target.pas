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
    FActive: Boolean;
    FAddress: String;
    FID: Integer;
    FPosition: Integer;
    FLastLogEntry: TLogEntry;
    FNumberOfErrors: Integer;
  public
    property Active: Boolean read FActive write FActive;
    property Address: String read FAddress write FAddress;
    property ID: Integer read FID write FID;
    property Position: Integer read FPosition write FPosition;
    property LastLogEntry: TLogEntry read FLastLogEntry write FLastLogEntry;
    property NumberOfErrors: Integer read FNumberOfErrors write FNumberOfErrors;
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
  FLastLogEntry := TLogEntry.Create;
end;

destructor TTarget.Destroy;
begin
  FreeAndNil(FLastLogEntry);
  inherited Destroy;
end;

end.
