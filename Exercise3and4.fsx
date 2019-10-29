(*

Nome: Carmine
Cognome: Caserio
Matricola: 490999
Corso: B

*)

(*

Si vuole realizzare un controllo grafico che consenta di simulare n corpi che interagiscono tra di loro. Il controllo
grafico dovra' mettere a disposizione, oltre ai tasti di controllo della vista per il pan, zoom, e rotazione (da
implementare esplicitamente, si puo' utilizzare la libreria di lightweight controls vista a lezione), la funzione di
disegno di una scatola e di una circonferenza. Una scatola altro non e' che un rettangolo che entra a far parte del
mondo. La circonferenza mostrera' inizialmente un vettore centrato che indichi la velocita' iniziale. Dovra' essere
possibile selezionare scatole e circonferenze per poterle eliminare.

*)

#load "LWC.fsx"

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.IO
open LWC

(* Questo tipo implementa il bottone da usare per fare il pan,
  lo zoom e la rotazione della vista nell'esercizio 3 e 4. *)
type LineButton() as this =
  inherit LWC()

  let mutable text = ""
  let mutable butbrush = Brushes.Red
  let mutable del = false

  do this.Size <- SizeF(100.f, 50.f)

  member this.Text with get() = text and set(v) = text <- v
  member this.ButBrush with get() = butbrush and set(v) = butbrush <- v
  member this.Del with get() = del and set(v) = del <- v

  override this.OnMouseDown e =
    base.OnMouseDown e
    (* Rimane giallo finche' non viene ripremuto, se si tratta
      del remove shape, altrimenti, ritorna rosso al mouse up. *)
    if (not del || this.ButBrush = Brushes.Red) then this.ButBrush <- Brushes.Yellow
    else this.ButBrush <- Brushes.Red

  override this.OnMouseUp e =
    base.OnMouseUp e
    if not del then this.ButBrush <- Brushes.Red

  override this.OnPaint e = 
      let g = e.Graphics
      g.FillRectangle(butbrush, RectangleF(PointF(), this.Size))
      g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Size.Width, this.Size.Height)
      (* usato per centrare la stringa da disegnare nel bottone *)
      let sz = g.MeasureString(text, this.Parent.Font)
      let szwm, szhm = sz.Width/2.f, sz.Height/2.f
      g.DrawString(text, this.Parent.Font, Brushes.White, PointF(this.Size.Width / 2.f - szwm, this.Size.Height / 2.f - szhm))



type MyPicture() as _this = 
    inherit LWC()
    let tol = 15.f
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

(* Ho definito qui il tipo Rect che viene utilizzato per 
  disegnare i rettangoli all'interno dell'esercizio 3 e 4.
  Ho definito un tipo apparte Rect perche' dato che eredita
  da LWC, e' coerente con il tipo Ball definito successivamente
  che eredita anch'esso da LWC. *)
type Line() =
  inherit LWC()

  let mutable initPt = PointF()
  let mutable finalPt = PointF()
  member this.InitPt
    with get() = initPt
    and set(v) = initPt <- v
  
  member this.finalPt
    with get() = finalPt
    and set(v) = finalPt <- v
  override this.HitTest p = gp.IsVisible( PointF(p.X - this.Size.Width / 2.f, p.Y - this.Size.Height / 2.f) )
  override this.OnPaint e = e.Graphics.DrawRectangle(Pens.Black, Rectangle(Point(), Size(int rect.Size.Width, int rect.Size.Height)))

(* Questo e' il tipo relativo alle circonferenze. Al suo
  interno sono definiti tutti i metodi per l'identificazione
  di una collisione l'aggiornamento della posizione e della
  velocita' legata all'oggetto di tipo Ball. *)
type Ball() as this =
  inherit LWC()

  (* Vettore che memorizza la velocita' dell'oggetto di
    tipo Ball attuale, l'unita' di misura e' pixel/sec *)
  let mutable speed = SizeF()
  let mutable radius = 15.f
  let mutable lastT = System.DateTime.Now
  let eps = 0.5f

  do this.Size <- SizeF(2.f * radius, 2.f * radius)

  member this.Speed with get() = speed and set(v) = speed <- v
  member this.Bounds = RectangleF(this.Location, this.Size)
  member this.Ctr with get() = PointF(this.Location.X + this.Size.Width / 2.f, this.Location.Y + this.Size.Height / 2.f)

  member this.UpdateSpeed(balls : Ball seq) =
    (* aggiorna la velocita' della Ball attuale,
      considerando il contributo della Ball b *)
    let vb (b : Ball) (vx : single, vy : single) =
      let cx, cy = b.Ctr.X - this.Ctr.X, b.Ctr.Y - this.Ctr.Y
      let sp = cx*vx + cy*vy
      let cr, vr = sqrt(cx*cx + cy*cy), sqrt(vx*vx+vy*vy)
      let cosa = sp / (cr*vr)
      let sina = sqrt(1.f - cosa*cosa)
      let vtb, votb = - vr * cosa, vr * sina
      let beta = atan2 cy cx
      let pi = single(System.Math.PI)
      let vtbx, vtby = vtb * cos(beta), vtb * sin(beta)
      let votbx, votby = votb * cos(pi / 2.f - beta), votb * sin(pi / 2.f - beta)
      SizeF(votbx + vtbx, votby + vtby)
    
    balls |> Seq.iter(fun b -> 
      if b <> this && this.CollideWith(b) then
        speed <- (vb b (speed.Width, speed.Height))
    )
  
  member this.UpdateSpeed(rects : Rect seq) =
    (* aggiorno la velocita' della Ball attuale,
      considerando il contributo del Rect r *)
    let vb (r : Rect) (vx : single, vy : single) =
      let cx, cy = eps - this.Ctr.X, eps - this.Ctr.Y
      let sp = cx*vx + cy*vy
      let cr, vr = sqrt(cx*cx + cy*cy), sqrt(vx*vx+vy*vy)
      let cosa = sp / (cr*vr)
      let sina = sqrt(1.f - cosa*cosa)
      let vtb, votb = - vr * cosa, vr * sina
      let beta = atan2 cy cx
      let pi = single(System.Math.PI)
      let vtbx, vtby = vtb * cos(beta), vtb * sin(beta)
      let votbx, votby = votb * cos(pi / 2.f - beta), votb * sin(pi / 2.f - beta)
      SizeF(votbx + vtbx, votby + vtby)
    
    rects |> Seq.iter(fun r -> 
      if this.CollideWith(r) then
        speed <- (vb r (speed.Width, speed.Height))
    )

  (* aggiorna la posizione della Ball attuale, analizzando,
    nel tempo, quanta distanza e' stata percorsa *)
  member this.UpdatePosition() =
    let t = System.DateTime.Now
    let dt = t - lastT
    let vx = speed.Width / 1000.f
    let vy = speed.Height / 1000.f
    let dx = vx * single(dt.TotalMilliseconds)
    let dy = vy * single(dt.TotalMilliseconds)
    this.Location <- PointF(this.Location.X + dx, this.Location.Y + dy)
    lastT <- t

  (* L'algoritmo utilizzato di seguito, viene utilizzato per
    controllare quando la Ball attuale la Ball che viene
    passata in input, collidono, ovvero, quando la distanza
    tra i due centri e' minore di due volte il radius della Ball. *)
  member this.CollideWith (b:Ball) =
    let sqdist (p1:PointF) (p2:PointF) =
      let dx, dy = p1.X - p2.X - 0.5f, p1.Y - p2.Y - 0.5f
      dx * dx + dy * dy
    let c1, c2 = this.Ctr, b.Ctr
    let d1, d2 = this.Size.Width / 2.f, b.Size.Width / 2.f
    sqdist c1 c2 <= (d1 + d2)*(d1 + d2)


  (* L'algoritmo utilizzato nella funzione seguente, serve
    per controllare quando avviene la collisione tra una
    ciconferenza (in movimento) e un rettangolo (fisso
    nello spazio). *)

  // FIX ME: le circonferenze interpretano i segmenti che
  // compongono il rettangolo, in rette, per cui, per esempio,
  // anche se, in realta', ci troviamo a destra del lato
  // all'estrema destra, ci sono collisioni con la prosecuzione
  // (inesistente) del lato superiore e di quello inferiore del rettangolo.
  member this.CollideWith (r:Rect) =
    let mutable res : bool option = None
    (* passo una linea qui definita da un punto iniziale e uno finale *)
    let lineCollision(i : PointF, f : PointF) =
      let sq x = x * x
      // calcolo la distanza euclidea tra i e f
      let lab = sqrt( sq (f.X - i.X) + sq (f.Y - i.Y) )
      // calcolo il vettore di direzione da i a f
      let d = PointF((f.X - i.X) / lab, (f.Y - i.Y) / lab)
      // Ora l'equazione della retta e': x = d.x*t + i.x, y = d.y*t + i.y, con 0 <= t <= 1.
      // calcolo il valore t del punto piu' vicino al centro del cerchio
      let t = d.X * (this.Ctr.X - i.X) + d.Y * (this.Ctr.Y - i.Y)
      // E questa e' la proiezione del centro del cerchio sulla retta da i a f
      // calcolo le coordinate del punto piu' vicino al centro del cerchio, E, sulla retta
      let e = PointF(t * d.X + i.X, t * d.Y + i.Y)
      // calcolo la distanza euclidea da E al centro del cerchio
      let lec = sqrt( sq (e.X - this.Ctr.X) + sq (e.Y - this.Ctr.Y) )
      if lec - radius <= 0.f then
        // calcolo la distanza da t al punto di intersezione col cerchio
        let dt = sqrt( sq radius - sq lec )
        // calcolo il primo punto di intersezione
        let fip = PointF((t - dt) * d.X + i.X, (t - dt) * d.Y + i.Y)
        // calcolo il secondo punto di intersezione
        let sip = PointF((t + dt) * d.X + i.X, (t + dt) * d.Y + i.Y)

        res <- Some((fip.X >= i.X && fip.X <= f.X) || (fip.Y >= i.Y && fip.Y <= f.Y) ||
                    (sip.X >= i.X && sip.X <= f.X) || (sip.Y >= i.Y && sip.Y <= f.Y))

      match res with
      | Some(a) ->
        let svd = a
        res <- None
        svd
      | None -> false
    (* L'algoritmo viene applicato a tutti i lati del rettangolo. *)
    lineCollision(PointF(r.Location.X + r.Size.Width, r.Location.Y), r.Location) ||
    lineCollision(PointF(r.Location.X, r.Location.Y + r.Size.Height), r.Location) ||
    lineCollision(PointF(r.Location.X + r.Size.Width, r.Location.Y), PointF(r.Location.X + r.Size.Width, r.Location.Y + r.Size.Height)) ||
    lineCollision(PointF(r.Location.X, r.Location.Y + r.Size.Height), PointF(r.Location.X + r.Size.Width, r.Location.Y + r.Size.Height))    

  override this.HitTest e =
    let sq x = x * x
    sq (e.X - (this.Location.X + this.Size.Width / 2.f)) + sq (e.Y - (this.Location.Y + this.Size.Height / 2.f)) < sq radius


type Shape = Ball | Rect | NoShape

(* Il tipo definisce il comportamento di tutti i bottoni e il loro
  comportamento al momento del mouse down. Inoltre definisce, il
  pan, zoom e rotazione della vista e l'algoritmo per l'aggiornamento
  delle posizioni delle circonferenze. *)
type Exercise3And4() as this =
  inherit LWContainer()
  
  let mutable w2v = new Matrix()
  let mutable v2w = new Matrix()

  let mutable pressX = false
  
  let mutable lines = new ResizeArray<Line>()
  let mutable pressRect : int option = None
  let mutable pressCirc : int option = None

  let mutable bip : Ball option = None
  let mutable key = Keys.W
  let mutable clickedNow = false

  let mutable dragStart = None
  let mutable positionRect = PointF()
  let mutable positionBall = PointF()

  let tBall = new Timer(Interval=30)
  let tBtns = new Timer(Interval=30)

  let toPoint(p : PointF) = Point(int p.X, int p.Y)
  let toRectangle (r : RectangleF) = Rectangle(int r.Location.X, int r.Location.Y, int r.Size.Width, int r.Size.Height)
  let toPointF(p : Point) = PointF(single p.X, single p.Y)
  let toRectangleF (r : Rectangle) = RectangleF(single r.Location.X, single r.Location.Y, single r.Size.Width, single r.Size.Height)

  let btns = [|
    new Button(Text="Up", Location=PointF(0.f, 0.f))
    new Button(Text="Left", Location=PointF(100.f, 0.f))
    new Button(Text="Down", Location=PointF(200.f, 0.f))
    new Button(Text="Right", Location=PointF(300.f, 0.f))
    new Button(Text="RotateC", Location=PointF(400.f, 0.f))
    new Button(Text="RotateCC", Location=PointF(500.f, 0.f))
    new Button(Text="ZoomIn", Location=PointF(600.f, 0.f))
    new Button(Text="ZoomOut", Location=PointF(700.f, 0.f))
    new Button(Text="Add Ball", Location=PointF(800.f, 0.f))
    new Button(Text="Add Rect", Location=PointF(900.f, 0.f))
    new Button(Text="Remove Shape", Location=PointF(1000.f, 0.f), Del=true)
  |]

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let updateBalls() =
    let initPt = transformP v2w (Point())
    let finPt = transformP v2w (Point(this.ClientSize.Width, this.ClientSize.Height))
    circ |> Seq.iter (fun c ->
      c.UpdatePosition() // Per la collision detection
      c.UpdateSpeed(circ)
      c.UpdateSpeed(rect)
      if c.Location.X < initPt.X || c.Location.X + c.Size.Width > finPt.X then
        c.Speed <- SizeF(- c.Speed.Width, c.Speed.Height)
      if c.Location.Y < initPt.Y || c.Location.Y + c.Size.Height > finPt.Y then
        c.Speed <- SizeF(c.Speed.Width, - c.Speed.Height)
    )

  let translateAll(dx : single, dy: single) =
    w2v.Translate(dx, dy)
    v2w.Translate(-dx, -dy, MatrixOrder.Append)

  let rotateAtAll(a : single, p : Point) =
    let cw = transformP v2w p

    w2v.Translate(-cw.X, -cw.Y)
    w2v.Rotate(a)
    w2v.Translate(cw.X, cw.Y)
    
    v2w.Translate(cw.X, cw.Y, MatrixOrder.Append)
    v2w.Rotate(-a, MatrixOrder.Append)
    v2w.Translate(-cw.X, -cw.Y, MatrixOrder.Append)

  let scaleAtAll(sx : single, sy : single, p : Point) =
    let cw = transformP v2w p
    w2v.Scale(sx, sy)
    v2w.Scale(1.f / sx, 1.f / sy, MatrixOrder.Append)
    let cwp = transformP v2w p
    w2v.Translate(cwp.X - cw.X, cwp.Y - cw.Y)
    v2w.Translate(cw.X - cwp.X, cw.Y - cwp.Y, MatrixOrder.Append)

  let transformations( cmd : Keys ) =
    let c = Point(this.ClientSize.Width / 2, this.ClientSize.Height / 2)
    match cmd with
    | Keys.W -> translateAll(0.f, -10.f)
    | Keys.S -> translateAll(0.f, 10.f)
    | Keys.A -> translateAll(-10.f, 0.f)
    | Keys.D -> translateAll(10.f, 0.f)
    | Keys.E -> rotateAtAll(-10.f, c)
    | Keys.Q -> rotateAtAll(10.f, c)
    | Keys.Z -> scaleAtAll(1.1f, 1.1f, c)
    | Keys.C -> scaleAtAll(1.f / 1.1f, 1.f / 1.1f, c)
    | Keys.X -> if pressX then pressX <- false else pressX <- true
    | _ -> ()

  
  do
    this.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint, true)

    tBall.Tick.Add(fun e -> updateBalls(); this.Invalidate())
    tBall.Start()

    tBtns.Tick.Add(fun _ -> key |> transformations; this.Invalidate())

    for b in btns do
      b.Parent <- this
      this.LWControls.Add(b)

    for i in 0 .. 1 .. btns.Length - 1 do
      btns.[i].MouseDown.Add(fun _ ->
        match i with
        | 0 -> key <- Keys.W
        | 1 -> key <- Keys.A
        | 2 -> key <- Keys.S
        | 3 -> key <- Keys.D
        | 4 -> key <- Keys.E
        | 5 -> key <- Keys.Q
        | 6 -> key <- Keys.Z
        | 7 -> key <- Keys.C
        | 8 -> shp <- Ball; clickedNow <- true
        | 9 -> shp <- Rect
        | 10 -> key <- Keys.X; shp <- NoShape; transformations key
        | _ -> ()
        if i >= 0 && i <= 7 then
          tBtns.Start()
      )

      btns.[i].MouseUp.Add(fun _ ->
        if i >= 0 && i <= 7 then
          tBtns.Stop()
        this.Invalidate()
      )

      btns.[10].ButBrush <- match pressX with true -> Brushes.Yellow | false -> Brushes.Red

  (* buffer è un oggetto Bitmap che servirà a realizzare il double
    buffering in modo da non avere flickering nel programma. La
    funzione updateBuffer si assicura che la Bitmap mantenga le
    stesse dimensioni del controllo. *)
  let mutable buffer : Bitmap = null
  let updateBuffer () =
    if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
      if buffer <> null then buffer.Dispose()
      buffer <- new Bitmap(this.Width, this.Height)

  (* vengono qui definite le funzioni necessarie per eliminare
    un oggetto dal resize array di Rect e di Ball *)
  let delRect() =
    this.LWControls.Remove(rect.[pressRect.Value]) |> ignore
    rect.RemoveAt(pressRect.Value) |> ignore
    this.Invalidate()

  let delBall() =
    this.LWControls.Remove(circ.[pressCirc.Value]) |> ignore
    circ.RemoveAt(pressCirc.Value) |> ignore
    this.Invalidate()

  let mkRectangleF(p1:PointF, p2:PointF) =
    let x1, y1 = min p1.X p2.X, min p1.Y p2.Y
    let x2, y2 = max p1.X p2.X, max p1.Y p2.Y
    RectangleF(x1, y1, x2 - x1, y2 - y1)

  let handleCommand k =
    k |> transformations
    this.Invalidate()

  override this.OnMouseDown e =
    base.OnMouseDown e
    (* Se il mouse down non e' avvenuto sui bottoni *)
    if e.Location.X > 1116 || e.Location.Y > 50 then

      let l = transformP v2w e.Location

      let rectHitTest(r : Rect, p : PointF) =
        let mutable ins = false
        if r.Location.X <= p.X && p.X <= r.Location.X + r.Size.Width then
          if r.Location.Y <= p.Y && p.Y <= r.Location.Y + r.Size.Height then
            ins <- true
        ins

      let hitHandleRect() =
        let mutable found = -1
        for i in 0 .. 1 .. rect.Count - 1  do
          if rectHitTest(rect.[i], l) then found <- i
        found

      let hitHandleCirc() =
        let mutable found = -1
        for i in 0 .. 1 .. circ.Count - 1 do
          if circ.[i].HitTest l then found <- i
        found

      (* uno dei due o entrambi vengono settati per
        vedere se esiste un elemento selezionato *)
      pressRect <- match hitHandleRect() with | -1 -> None | f -> Some(f)
      pressCirc <- match hitHandleCirc() with | -1 -> None | f -> Some(f)
      (* se non e' stata premuta la X, per eliminare
        l'oggetto, allora si gesisce la creazione di un
        nuovo oggetto. Qui, al mouse down, il rettangolo
        non viene creato, ma viene solo cominciata la
        sua definizione, la circonferenza, invece viene
        creata qui e viene successivamente definita la
        sua velocita' iniziale. *)
      if not pressX && not clickedNow then
        match shp with
        | Rect ->
          dragStart <- Some(l)
          positionRect <- l
          this.Invalidate()
        | Ball ->
          let b = new Ball()
          dragStart <- Some(l)
          b.Location <- PointF(l.X - b.Size.Width / 2.f, l.Y - b.Size.Height / 2.f)
          bip <- Some(b)
          circ.Add(b)
          positionBall <- l
          this.Invalidate()
        | NoShape -> ()
      (* si definisce qui l'eliminazione del
        rettangolo o della circonferenza *)
      elif pressRect.IsSome && pressX then delRect()
      elif pressCirc.IsSome && pressX then delBall()

    this.Invalidate()

  override this.OnMouseMove e =
    base.OnMouseMove e

    let l = transformP v2w e.Location

    let h() = updateBalls(); this.Invalidate()
    (* si aggiornano le posizioni delle circonferenze
      o quando il rettangolo e' in fase di creazione,
      o quando dopo che la circonferenza C e' stata
      creata, l'utente decide la velocita' iniziale
      di C. *)
    if shp = Rect && dragStart.IsSome then positionRect <- l; h()
    elif shp = Ball && bip.IsSome then dragStart <- Some(l); h()

  override this.OnMouseUp e =
    base.OnMouseUp e

    let l = transformP v2w e.Location
    if shp = Rect && dragStart.IsSome then
      (* Termina la fase di creazione del rettangolo,
        in cui si crea l'oggetto e lo si aggiunge nel
        ResizeArray dei rettangoli. *)
      positionRect <- l
      let r = new Rect()
      r.Rectangle <- mkRectangleF(dragStart.Value, l)
      rect.Add(r)
      dragStart <- None
      this.Invalidate()
    elif shp = Ball && bip.IsSome then
      (* Viene stabilita la velocita' iniziale
        della circonferenza. *)
      let b = bip.Value
      positionBall <- l
      dragStart <- None
      let x0, y0 = b.Location.X + b.Size.Width / 2.f, b.Location.Y + b.Size.Height / 2.f
      b.Speed <- SizeF((l.X - x0) / 2.f, (l.Y - y0) / 2.f)
      bip <- None
      this.Invalidate()
    clickedNow <- false

  (* Al key down, viene gestita la pressione del tasto e
    al key up se il tasto premuto era la X, viene resettata
    a false la variabile che tiene traccia del tasto premuto. *)
  override this.OnKeyDown e = base.OnKeyDown e; handleCommand e.KeyCode
  override this.OnKeyUp e = base.OnKeyUp e; if e.KeyCode = Keys.X then pressX <- false
  override this.OnPaintBackground e = ()

  (* Viene aggiornato il buffer, si prende il contesto grafico 
    del buffer e lo si riempie prima del colore dello sfondo,
    e viene successivamente chiamata la onPaint del parent
    relativa al rettangolo in cui sono presenti bottoni. *)
  override this.OnPaint e =
    base.OnPaint e
    updateBuffer()
    let g = Graphics.FromImage(buffer)
    g.SmoothingMode <- SmoothingMode.HighQuality
    let vg = e.Graphics
    use bg = new SolidBrush(this.BackColor)
    g.FillRectangle(bg, 0, 0, buffer.Width, buffer.Height)
    base.OnPaint(new PaintEventArgs(g, new Rectangle(0, 0, 1116, 50)))

    (* viene settata la trasformazione globale geometrica alla
      matrice da mondo a vista per aggiornare quest'ultima. *)
    let ctx = g.Transform
    g.Transform <- w2v
    (* se dragStart non e' vuoto, allora nel caso dei rettangoli,
      si disegna il rettangolo prima che questo venga inserito nel
      ResizeArray, nel caso delle circonferenze, si disegna la linea
      che indica la velocita' iniziale. Successivamente, si gestisce
      il disegno dei rettangoli e delle circonferenze presenti
      all'interno dei ResizeArray. *)
    if dragStart.IsSome then
      match shp with
      | Rect -> g.DrawRectangle(Pens.Black, mkRectangleF(dragStart.Value, positionRect) |> toRectangle)
      | Ball -> g.DrawLine(Pens.Black, dragStart.Value, positionBall)
      | NoShape -> ()

    if not(Seq.isEmpty(rect)) then rect |> Seq.iter(fun r -> g.DrawRectangle(Pens.Black, r.Rectangle |> toRectangle))
    if not(Seq.isEmpty(circ)) then circ |> Seq.iter(fun c -> g.DrawEllipse(Pens.Black, c.Bounds); g.FillEllipse(Brushes.Cyan, c.Bounds))
    (* si ripristina la trasformazione legata
      inizialmente al contesto grafico. *)
    g.Transform <- ctx
    (* si disegna su schermo il buffer riempito
      precedentemente dalla variabile g. *)
    vg.DrawImage(buffer, 0, 0)

let f = new Form(TopMost=true, Text="Exercise 3 and 4", Size=Size(1116, 600))
f.Show()

let e = new Exercise3And4(Dock=DockStyle.Fill)
f.Controls.Add(e)

e.Focus()