
#Edit Box in R

require(tcltk)
tt<-tktoplevel()
Name <- tclVar("Anonymous")
entry.Name <-tkentry(tt,width="20",textvariable=Name)
tkgrid(tklabel(tt,text="Please enter your first name."))
tkgrid(entry.Name)
OnOK <- function()
{
  NameVal <- tclvalue(Name)
  tkdestroy(tt)
  msg <- paste("You have a nice name,",NameVal)
  tkmessageBox(message=msg)
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt)