@echo off

rd/s/q bin lib databases Templates Examples sources
xcopy /e/h/y/q/i ..\..\bin       bin
xcopy /e/h/y/q/i ..\..\lib       lib
xcopy /e/h/y/q/i ..\..\databases databases
xcopy /e/h/y/q/i ..\..\Templates Templates
xcopy /e/h/y/q/i ..\..\Examples  Examples

svn export ..\..\sources sources
xcopy /e/h/y/q/i ..\..\sources\corba\orb\iop-protocol sources\corba\orb\iop-protocol 
xcopy /e/h/y/q/i ..\..\sources\corba\orb\ir-stubs sources\corba\orb\ir-stubs
xcopy /e/h/y/q/i ..\..\sources\corba\orb\ir-protocol sources\corba\orb\ir-protocol
xcopy /e/h/y/q/i ..\..\sources\corba\services\naming\naming-protocol sources\corba\services\naming\naming-protocol
xcopy /e/h/y/q/i ..\..\sources\corba\services\naming\naming-stubs sources\corba\services\naming\naming-stubs
xcopy /e/h/y/q/i ..\..\sources\corba\services\naming\naming-skeletons sources\corba\services\naming\naming-skeletons
xcopy /e/h/y/q/i ..\..\sources\runtime-manager\debugger-nub\protocol sources\runtime-manager\debugger-nub\protocol
xcopy /e/h/y/q/i ..\..\sources\runtime-manager\debugger-nub\skeletons sources\runtime-manager\debugger-nub\skeletons
xcopy /e/h/y/q/i ..\..\sources\runtime-manager\debugger-nub\stubs sources\runtime-manager\debugger-nub\stubs

makensis fundev.nsi
