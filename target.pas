unit target;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TDetails = class
    Time: TDateTime;
    Result: Boolean;
    PingTime: Integer;
    Interval: Cardinal;
  end;

  TDetailsList = specialize TFPGObjectList<TDetails>;

  { TTarget }

  TTarget = class(TObject)
  private
    FAdress: String;
    FHistory: TDetailsList;
  public
    property Adress: String read FAdress write FAdress;
    property History: TDetailsList read FHistory write FHistory;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTarget }

constructor TTarget.Create;
begin
  inherited Create;
  FHistory:= TDetailsList.Create;
end;

destructor TTarget.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

end.

