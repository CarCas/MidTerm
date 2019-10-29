#load "lwc.fsx"

open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open Lwc

type Line() as _this =
    let mutable firstPoint = PointF()
    let mutable endPoint = PointF()
    member this.FirstPoint with
        set(v) = firstPoint <- v and
        get() = firstPoint
    member this.EndPoint with
        set(v) = endPoint <- v and
        get() = endPoint

type MyPicture() as _this = 
    inherit LWControl()
    let tol = 15.f
    let gp = new GraphicsPath()
    let mutable reg = new Region()
    let p = new Pen(Brushes.LimeGreen, 20.f)
    let mutable location = new PointF(300.f, 100.f)
    let mutable size = SizeF()
    let mutable pic = new PictureBox(
                            Image=Image.FromFile( __SOURCE_DIRECTORY__ +  @"/corgi.jpg"),
                            Anchor=AnchorStyles.None)
    
    let mutable chosen : bool option = None
    let mutable addingSource = PointF()
    let mutable addingDestination = PointF()
    member this.Width with get() = single pic.Image.Size.Width * 0.8f
    member this.Height with get() = single pic.Image.Size.Height * 0.8f
    
        
        
    member this.Location with get() = location and set(v) = location <- v

    override this.OnPaint e = 
        let g = e.Graphics
        let width = pic.Image.Size.Width
        let height = pic.Image.Size.Height
        g.DrawImage(pic.Image,
                RectangleF(
                    location,
                    SizeF(single width * 0.8f, single height * 0.8f)
                ))
        

and LineButton() =
    inherit LWControl()

    let mutable x = 0.f
    let mutable y = 0.f
    let mutable text = ""
    let mutable controlledPic = MyPicture()
    let mutable linesCreated = new ResizeArray<Line>()
    let mutable lineType = 0
    member this.X with set(v) = x <- v
    member this.Y with set(v) = y <- v
    member this.Text with set(v) = text <- v

    member this.Type with
        set(v) = lineType <- v
    member this.Picture with
        set(v) = controlledPic <- v and
        get() = controlledPic
    
    override this.OnMouseDown e =
        let mutable firstPoint = PointF(-1.f, -1.f)
        let mutable endPoint = PointF(-1.f, -1.f)
        printfn "%d" linesCreated.Count
        match linesCreated.Count with
            0 -> do
                printfn "Nessuna Linea!!"
                match lineType with 
                0 -> do 
                    firstPoint <- PointF(controlledPic.Location.X + controlledPic.Width / 2.f, controlledPic.Location.Y)
                    endPoint <- PointF(controlledPic.Location.X + controlledPic.Width / 2.f, controlledPic.Location.Y + controlledPic.Height)
                | 1 -> do
                    firstPoint <- PointF(controlledPic.Location.X, controlledPic.Location.Y + controlledPic.Height / 2.f)
                    endPoint <- PointF(controlledPic.Location.X + controlledPic.Width,  controlledPic.Location.Y + controlledPic.Height / 2.f)
                | _ -> ()
            | _ -> do
                let lastLineIdx = linesCreated.Count - 1
                let lastLineFP = linesCreated.[lastLineIdx].FirstPoint
                let lastLineEP = linesCreated.[lastLineIdx].EndPoint
                match lineType with 
                0 -> do 
                    firstPoint <- PointF(lastLineFP.X / 2.f, controlledPic.Location.Y)
                    endPoint <- PointF(lastLineEP.X / 2.f, controlledPic.Location.Y + controlledPic.Height)
                | 1 -> do
                    firstPoint <- PointF(controlledPic.Location.X, lastLineFP.Y / 2.f)
                    endPoint <- PointF(controlledPic.Location.X + controlledPic.Width,  lastLineEP.Y / 2.f)
                | _ -> ()
        match firstPoint.X with
            -1.f -> ()
            | _ -> do
                let newLine = new Line(FirstPoint = firstPoint, EndPoint = endPoint)
                linesCreated.Add(newLine)
                this.Invalidate()

            
    override this.OnPaint e =
        let g = e.Graphics
        g.DrawRectangle(Pens.Blue, x, y, 120.f, 30.f)
        g.DrawString(text, new Font("Arial", 10.f), Brushes.Blue, PointF(x, y))
        for i in 0 .. linesCreated.Count - 1 do
            let line = linesCreated.[i]
            g.DrawLine(Pens.Red, line.FirstPoint, line.EndPoint)


type Puzzle() as _this =
    inherit LWContainer()

    let mutable myPic = new MyPicture()

    let mutable butt = new LineButton()

    







let form = new Form(
            WindowState = FormWindowState.Maximized,
            DesktopBounds=Rectangle(0, 0, Screen.PrimaryScreen.Bounds.Width, Screen.PrimaryScreen.Bounds.Height)
            )

let cont = new LWContainer(Dock = DockStyle.Fill)
form.Controls.Add(cont)
cont.Select()

let puzzlePic = new MyPicture(Parent=cont)
cont.LWControls.Add(puzzlePic)

let horizBut = new LineButton(X = 0.f, Y = 0.f, Text = "Linea Orizzontale", Type = 1)
let verBut = new LineButton(X = 0.f, Y = 40.f, Text = "Linea Verticale", Type = 0)
horizBut.Picture <- puzzlePic
verBut.Picture <- puzzlePic
horizBut.Select <- true
verBut.Select <- true
cont.LWControls.Add(horizBut)
cont.LWControls.Add(verBut)

//let but = new NewHandleButton(Parent=cont)
//cont.LWControls.Add(but)

form.Show()