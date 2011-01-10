Module:       DUIM-formatting-internals
Synopsis:     DUIM formatted output
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro formatting-table
  { formatting-table (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-table-body = method (?record) ?body end;
           do-formatting-table(?sheet, formatting-table-body, ?options)
         end }
  { formatting-table (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-table-body = method (_record) ignore(_record); ?body end;
           do-formatting-table(?sheet, formatting-table-body, ?options)
         end }
end macro formatting-table;

define macro formatting-row
  { formatting-row (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-row-body = method (?record) ?body end;
           do-formatting-row(?sheet, formatting-row-body, ?options)
         end }
  { formatting-row (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-row-body = method (_record) ignore(_record); ?body end;
           do-formatting-row(?sheet, formatting-row-body, ?options)
         end }
end macro formatting-row;

define macro formatting-column
  { formatting-column (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-column-body = method (?record) ?body end;
           do-formatting-column(?sheet, formatting-column-body, ?options)
         end }
  { formatting-column (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-column-body = method (_record) ignore(_record); ?body end;
           do-formatting-column(?sheet, formatting-column-body, ?options)
         end }
end macro formatting-column;

define macro formatting-cell
  { formatting-cell (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-cell-body = method (?record) ?body end;
           do-formatting-cell(?sheet, formatting-cell-body, ?options)
         end }
  { formatting-cell (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-cell-body = method (_record) ignore(_record); ?body end;
           do-formatting-cell(?sheet, formatting-cell-body, ?options)
         end }
end macro formatting-cell;


define macro formatting-items
  { formatting-items (?record:variable = ?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-items-body = method (?record) ?body end;
           do-formatting-items(?sheet, formatting-items-body, ?options)
         end }
  { formatting-items (?sheet:variable, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let formatting-items-body = method (_record) ignore(_record); ?body end;
           do-formatting-items(?sheet, formatting-items-body, ?options)
         end }
end macro formatting-items;

