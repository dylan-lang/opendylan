{
APPLICATION: Hints Manuals Part Number Generator
PURPOSE:
        This applet calculate part numbers based on the rules devised by Andy,
        which themselves are modified versions of the doc group standard rules.

VERSION:1.0

BUGS:   None known

TO DO:
   Support for non-Hints part numbers
   Add intelligence to list members, so that only applicable manuals displayed
   once a product has been chosen
   Make combo boxes list boxes, since you never need to write in them.
   Add support for Online Help part numbers (do this once you have intelligent list items)
   Make ShowError display a real error message dialog with a sensible title.
}
unit partno;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ProductBox: TComboBox;
    ManualBox: TComboBox;
    VersionBox: TEdit;
    GenerateButton: TButton;
    PartNoBox: TEdit;
    GroupBox1: TGroupBox;
    procedure GenerateButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function ShowError : string;
{Display an error dialog and set partnum to nullif some information is missing}
begin
     ShowMessage('You must supply a product, version number, and manual name');
     ShowError := '';
end;

function stripped (stripchar : char; str : string) : string;
{
 Generic function to strip stripchar from str. Found at HelpMakers site.
 Should probably go into a library of string handling routines
}
var
   tmpstr : string;

begin
     tmpstr := str;
     while pos(stripchar, tmpstr) > 0 do
           delete (tmpstr, pos(stripchar, tmpstr), 1);
     stripped := tmpstr;
end;

function MakePartNo( product, manual : integer; version : string) : string;
{
 Create part number from chosen product, manual, version, and the current date.
 This passes the ItemIndex from the appropriate combo boxes, but something
 neater might be possible.
}
var
   ProdCode, ManCode, VerCode, RevDate : string;
begin
case product of
     0:   ProdCode := 'WA';  {Watson}
     1:   ProdCode := 'PC';  {PowerCase}
     2:   ProdCode := 'CC';  {CaseCall}
     else ProdCode := '';
end;
case manual of
     0:   ManCode := 'UW';   {Using Watson}
     1:   ManCode := 'GSEW'; {Getting Started With Elementary Watson}
     2:   ManCode := 'GSW';  {Getting Started With Watson}
     3:   ManCode := 'MRD';  {Modeling and Representing Data}
     4:   ManCode := 'WMAP'; {Watson Mapping}
     5:   ManCode := 'WRME'; {Watson Read Me}
     6:   ManCode := 'PCT';  {PowerCase Typing}
     7:   ManCode := 'PCIX'; {PowerCase Indexing}
     8:   ManCode := 'PCA';  {PowerCase Admin}
     9:   ManCode := 'PCO';  {PowerCase Overview}
     10:  ManCode := 'PCIL'; {PowerCase Installation}
     11:  ManCode := 'PCRM'; {PowerCase Read Me}
     12:  ManCode := 'UCC';  {Using CaseCall}
     13:  ManCode := 'CCRM'; {CaseCall Read Me}
     else ManCode := '';
end;
if (version = '') or (ProdCode = '') or (ManCode = '') then
    MakePartNo := ShowError
else begin
     VerCode := stripped('.',version);  {Strip periods from version number}
     RevDate := FormatDateTime ('" (Last revised" dd mmm yy' + ')', Date);
     MakePartNo :=  ProdCode + '-' + VerCode + '-' + ManCode + RevDate; {Return result}
    end;
end;

procedure TForm1.GenerateButtonClick(Sender: TObject);
var
   product, manual : integer;
   version, partnum : string;

begin
     product := ProductBox.ItemIndex;
     manual := ManualBox.ItemIndex;
     version := VersionBox.Text;
     partnum := MakePartNo(product, manual, version);
     PartNoBox.Text := partnum;
end;

end.
