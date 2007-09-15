namespace LScriptTestHost
{
    partial class TestHostForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose (bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose ();
            }
            base.Dispose (disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent ()
        {
            this.TestHostBrowser = new System.Windows.Forms.WebBrowser ();
            this.SuspendLayout ();
            // 
            // TestHostBrowser
            // 
            this.TestHostBrowser.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TestHostBrowser.Location = new System.Drawing.Point (0, 0);
            this.TestHostBrowser.MinimumSize = new System.Drawing.Size (20, 20);
            this.TestHostBrowser.Name = "TestHostBrowser";
            this.TestHostBrowser.Size = new System.Drawing.Size (632, 446);
            this.TestHostBrowser.TabIndex = 0;
            this.TestHostBrowser.Url = new System.Uri ("C:\\jrm-code-project\\LScript\\IEHost.html", System.UriKind.Absolute);
            // 
            // TestHostForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF (6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size (632, 446);
            this.Controls.Add (this.TestHostBrowser);
            this.Name = "TestHostForm";
            this.Text = "ScriptTestHost";
            this.ResumeLayout (false);

        }

        #endregion

        private System.Windows.Forms.WebBrowser TestHostBrowser;
    }
}

