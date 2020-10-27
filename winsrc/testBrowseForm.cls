 
 /*------------------------------------------------------------------------
    File        : testBrowseForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Mon Sep 21 13:18:39 CEST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS winsrc.testBrowseForm INHERITS Form: 
  
  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE webBrowser1 AS System.Windows.Forms.WebBrowser NO-UNDO.

    
  CONSTRUCTOR PUBLIC testBrowseForm (  ):
    
    
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    
    openWebBrowser(  ).
    
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

  METHOD PRIVATE VOID InitializeComponent(  ):
    
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:webBrowser1 = NEW System.Windows.Forms.WebBrowser().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* webBrowser1 */
    /*  */
    THIS-OBJECT:webBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
    THIS-OBJECT:webBrowser1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:webBrowser1:MinimumSize = NEW System.Drawing.Size(20, 20).
    THIS-OBJECT:webBrowser1:Name = "webBrowser1".
    THIS-OBJECT:webBrowser1:Size = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:webBrowser1:TabIndex = 0.
    /*  */
    /* testBrowseForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:webBrowser1).
    THIS-OBJECT:Name = "testBrowseForm".
    THIS-OBJECT:Text = "testBrowseForm".
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID openWebBrowser(  ):
    
   webBrowser1:Navigate("https://progress.com").    

  END METHOD.

  DESTRUCTOR PUBLIC testBrowseForm ( ):

  END DESTRUCTOR.

END CLASS.