###
The MIT License

Copyright (c) 2016 Jason S. Ku

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

TO DO
[ ] restrict to flat filling
[ ] impliment collision detection
[ ] add resizing for mobile devices
[ ] add interface (touch) for mobile devices

2016-01-05
[x] make talk examples

2016-01-04
[x] impiment edge insetting

2015-12-22
[x] fix OBJ to fit specification

2015-11-25 to 2015-12-19
[x] refactor code for polyhedron viewing/selection
[x] impliment direction/critical selection
[x] impliment random hole generation
[x] add interface buttons
[x] import/export obj file

2015-11-24
[x] adjust scale to actual diameter
[x] test crease directions methods
[x] impliment hole splitting navigation
[x] impliment non-critical hole splitting
[x] impliment auto-fill holes
[x] added flat example

2015-11-23
[x] fix scale/center for view
[x] fix @points
[x] fully test visibility
[x] figure out static properties
[x] general refactorization
[x] associate selected with each hole
###

###
Global Variables
###

EPS = Math.pow(10,-13)

DELETE =  8
TAB    =  9
RETURN = 13
SPACE  = 32
LEFT   = 37
UP     = 38
RIGHT  = 39
DOWN   = 40
KEY_A  = 65
KEY_C  = 67
KEY_H  = 72
KEY_J  = 74
KEY_K  = 75
KEY_L  = 76
KEY_N  = 78
KEY_S  = 83
KEY_T  = 84
COMMA  = 188
PERIOD = 190
TILDE  = 192

BLACK  = '#000000'
RED    = '#ff0000'
GREEN  = '#00ff00'
BLUE   = '#0000ff'
YELLOW = '#ffff00'

INPUT = [
  {vertices:
     [[ 11,   5,  9, 4,  0]
      [ 11, 2.5,  9, 2,1.5]
      [ 11,   0,7.5, 0,1.5]
      [ 11,-2.5,  9,-2,1.5]
      [ 11,  -5,  9,-4,  0]
      [  6,  -5,  5,-1,  0]
      [  1,  -5,  1,-4,  0]
      [  0,  -5,  2,-4,  0]
      [  1,   0,  1, 0,  3]
      [ -1,   0, -1, 0,  3]
      [ -1,  -5, -1,-4,  0]
      [ -6,  -5, -5,-1,  0]
      [-11,  -5, -9,-4,  0]
      [-11,   0, -9, 0,  3]
      [-11,   5, -9, 4,  0]
      [ -6,   5, -5, 1,  0]
      [ -1,   5, -1, 4,  0]
      [  1,   5,  1, 4,  0]
      [  6,   5,  5, 1,  0]]
   faces: [[0..18]]}
  {vertices:
     [[0,0,0,0,0]
      [0,1,1,0,0]
      [1,1,0,0,0]
      [1,0,1,0,0]]
   faces: [[0..3]]}
  {vertices:
     [[0,2,0,2,0]
      [2,0,2,0,0]
      [1,-3,1,3/Math.sqrt(2),3/Math.sqrt(2)]
      [0,0,0,0,0]
      [-5,1,5/Math.sqrt(2),1,5/Math.sqrt(2)]]
   faces: [[0..4]]}
  {vertices:
     [[0,0,0, 0,0]
      [0,3,2, 2,1]
      [3,3,0, 0,2]
      [3,0,2,-2,1]]
   faces: [[0..3]]}
  {vertices:
     [[0,1,0,1,0]
      [1,1,1,1,0]
      [3,1,1-2/Math.sqrt(2),1,2/Math.sqrt(2)]
      [1,0,1,0,0]
      [0,0,0,0,0]
      [-2,0,2/Math.sqrt(2),0,2/Math.sqrt(2)]]
   faces: [[0..5]]}
  {vertices:
     [[ 5, 5, 4, 4, 0]
      [ 5, 0, 4, 0, 3]
      [ 5,-5, 4,-4, 0]
      [ 0,-5, 0,-1, 0]
      [-5,-5,-4,-4, 0]
      [-5, 0,-4, 0, 3]
      [-5, 5,-4, 4, 0]
      [ 0, 5, 0, 1, 0]]
   faces: [[0..7]]}
  {vertices:
     [[ 5, 5, 4, 4, 0]
      [ 5, 0, 4, 0, 3]
      [ 5,-5, 4,-4, 0]
      [ 0,-5, 0,-1, 0]
      [-5,-5,-4,-4, 0]
      [-5, 0,-4, 0, 3]
      [-5, 5,-4, 4, 0]
      [ 0, 5, 0, 1, 0]]
   faces: [[0..7]]}
  {vertices:
     [[0,0,0, 0,0]
      [0,3,2, 2,1]
      [3,3,0, 0,2]
      [3,0,2,-2,1]]
   faces: [[0..3]]}
  {vertices:
     [[ 2, 0, 1, 0, 1]
      [ 1,-1, 1,-1, 0]
      [ 0,-2, 0,-1,-1]
      [-1,-1,-1,-1, 0]
      [-2, 0,-1, 0, 1]
      [-1, 1,-1, 1, 0]
      [ 0, 2, 0, 1,-1]
      [ 1, 1, 1, 1, 0]]
   faces: [[0..7]]}
  {vertices:
     [[1,2,1,2,0]
      [2,1,1,1,1]
      [1,0,1,0,0]
      [0,1,0,1,0]]
   faces: [[0..3]]}
  {vertices:
     [[ 1, 1, 1, 1, 0]
      [ 1, 0, 1, 0, 0]
      [ 1,-1, 1,-1, 0]
      [ 0,-1, 0,-1, 0]
      [-1,-1,-1,-1, 0]
      [-1, 0,-1, 0, 0]
      [-1, 1,-1, 1, 0]
      [ 0, 1, 0, 1, 0]]
   faces: [[0..7]]}
  {vertices:
     [[0,0,0,0,0]
      [0,1,0,1,0]
      [1,0,1,0,0]]
   faces: [[0..2]]}
 ]
###
  {vertices:
     [[ 0, 0, 0, 0, 0]
      [ 0, 2, 0, 2, 0]
      [ 0, 3, 0, 1, 0]
      [ 0, 5, 0, 3, 0]
      [ 2, 3, 2, 1, 0]
      [ 3, 2, 1, 2, 0]
      [ 5, 0, 3, 0, 0]
      [ 3, 0, 1, 0, 0]
      [ 2, 0, 2, 0, 0]]
   faces: [[0..8]]}
  {vertices:
     [[0,0,0,0,0]
      [0,2,2,0,0]
      [2,2,0,0,0]
      [2,1,1,0,0]
      [2,0,0,0,0]
      [1,0,1,0,0]]
   faces: [[0..5]]}
  {vertices:
     [[ 0, 0, 0, 0, 0]
      [-2, 2, -Math.sqrt(2), Math.sqrt(6), 0]
      [-2 + 1/Math.sqrt(3), 2 + 1/Math.sqrt(3), -1/Math.sqrt(2), 1/Math.sqrt(6) + Math.sqrt(6), 0]
      [-2 + 2/Math.sqrt(3), 2 + 2/Math.sqrt(3), 0, Math.sqrt(6), 0]
      [0, 4, 0, Math.sqrt(6) - 2 * (Math.sqrt(2) - 2 / Math.sqrt(6)), 0]
      [ 2 - 2/Math.sqrt(3), 2 + 2/Math.sqrt(3), 0, Math.sqrt(6), 0]
      [ 2 - 1/Math.sqrt(3), 2 + 1/Math.sqrt(3),  1/Math.sqrt(2), 1/Math.sqrt(6) + Math.sqrt(6), 0]
      [ 2, 2,  Math.sqrt(2), Math.sqrt(6), 0]]
   faces: [[0..8]]}
###

SLIDE_TEXT = ('' for i in INPUT.concat INPUT)
SLIDE_TEXT[INPUT.length - 1] =
  'JMM 2016/01/06 --- Interface --- Problem --- Motivation --- Past Work --- Necessary Condition  ---  New Work (1) 3D Implimentation (2) All Isometries (3) Intersection'
SLIDE_TEXT[ 0] = 'Nonexpansive triangle: isometry always exists and is unique'
SLIDE_TEXT[ 1] = 'Flat maps: isometry always exists and is unique'
SLIDE_TEXT[ 2] = 'Critical maps: isometry always exists and is unique'
SLIDE_TEXT[ 3] = 'Can choose among options'
SLIDE_TEXT[ 4] = 'Contractive maps: infinite isometries exist --- Idea: locally satisfy vertex and recurse --- One dimension of local single crease solutions'
SLIDE_TEXT[ 5] = 'Repeat procedure with more complex boundaries --- Implimentation can construct random isometries'
SLIDE_TEXT[ 6] = 'Edge insetting, parameterized space of all triangles that can bound an edge'
SLIDE_TEXT[ 7] = 'Self intersection: Map must be unknot --- Necessary/sufficient conditions seem nontrivial'
SLIDE_TEXT[ 8] = 'Proposition: All paper boundaries folded at only four points can fold without intersection'
SLIDE_TEXT[ 9] = 'Some paper boundaries folded at five points cannot be folded without intersection'
SLIDE_TEXT[10] = 'Future Approach: Search space of solutions to find isometries free from intersection --- This example unique up to embedding with old method'

###
GENERAL POLYHEDRA VIEWER CLASSES
###

class Point
 
  @free: 0

  constructor: (@x, @y, @z = 0) -> @id = Point.free++

  add:  (v) -> new Point(@x + v.x, @y + v.y, @z + v.z)
  plus: (s) -> new Point(@x + s, @y + s, @z + s)
  mul:  (s) -> new Point(@x * s, @y * s, @z * s)
  div:  (s) -> @mul(1.0 / s)
  sub:  (v) -> @add(v.mul(-1))
  dot:  (v) -> (@x * v.x) + (@y * v.y) + (@z * v.z)
  dist: (v) -> Math.sqrt(@dist_sq v)
  dist_sq: (v) -> @sub(v).mag_sq()
  is_zero: () -> @mag() is 0
  neg_y:  () -> new Point(@x, -@y, @z)
  mag_sq: () -> @dot @
  mag: () -> Math.sqrt @mag_sq()
  dir: () -> @div @mag()

  cross: (v) ->
    new Point(
      (@y * v.z) - (@z * v.y)
      (@z * v.x) - (@x * v.z)
      (@x * v.y) - (@y * v.x)
    )

  angle: (u, v) ->
    u = u.sub(@).dir()
    v = v.sub(@).dir()
    Math.acos(u.dot v)

  rotate: (axis, ang) ->
    @mul(Math.cos(ang)).add(
      axis.cross(@).mul(Math.sin(ang))).add(
      axis.mul(@dot(axis)).mul(1-Math.cos(ang)))

  perp: () ->
      p = new Point 1, 0, 0
      p = new Point 0, 1, 0 if @cross(p).is_zero()
      @cross(p).dir()
  
  local_normal: (next, prev) ->
    normal = next.sub(@).cross(prev.sub(@))
    if normal.mag_sq() > EPS
      return normal.dir()
    return next.sub(@).perp()

  turns_right: (u, v, n = new Point 0, 0, 1) -> u.sub(@).cross(v.sub @).dot(n) > 0
    
  exterior_straddle: (next, prev, q, normal) ->
    ((@turns_right next, q, normal) and
     (@turns_right q, prev, normal))

  exterior_right: (next, prev, q, normal) ->
    ((@turns_right q, prev, normal) and
     (@turns_right prev, next, normal))

  exterior_left: (next, prev, q, normal) ->
    ((@turns_right next, q, normal) and
     (@turns_right prev, next, normal))

  between: (u, v) ->
    a = @sub u
    b = @sub v
    return true if (a.mag() < EPS) or (b.mag() < EPS)
    a.dir().dot(b.dir()) + 1 < EPS

  intersects: (q, u, v, normal) ->
    (( @turns_right(u, q, normal) is  @turns_right(q, v, normal)) and
     (u.turns_right(@, v, normal) is u.turns_right(v, q, normal)))
  
  @centroid: (ps) ->
    out = new Point 0, 0, 0
    out = out.add(p) for p in ps
    out.div(ps.length)

  @center: (ps) ->
    max = (new Point 1, 1, 1).div(-EPS)
    min = (new Point 1, 1, 1).div(EPS)
    for p in ps
      max[c] = Math.max p[c], max[c] for c in ['x', 'y', 'z']
      min[c] = Math.min p[c], min[c] for c in ['x', 'y', 'z']
    max.add(min).div 2

  @normal: (ps) ->
    normal = null
    for p, i in ps
      next = ps[(i + 1) % ps.length]
      prev = ps[(i - 1 + ps.length) % ps.length]
      temp = p.local_normal(next, prev)
      if temp?
        return null if normal? and (1 - Math.abs(normal.dot(temp)) > EPS)
        normal = temp
    normal

  @visible: (ps, i, p, normal) ->
    ###
    returns if line from p to ps[i] is interior to ps projected on normal
    ###
    next = ps[(i + 1) % ps.length]
    prev = ps[(i - 1 + ps.length) % ps.length]
    return false if p.sub(ps[i]).mag() < EPS
    return false if p.sub(next).mag() < EPS
    return false if p.sub(prev).mag() < EPS
    return false if ps[i].exterior_straddle next, prev, p, normal
    return false if ps[i].exterior_left next, prev, p, normal
    return false if ps[i].exterior_right next, prev, p, normal
    for q, j in ps when (q isnt ps[i]) and (q.sub(p).mag() > EPS)
      return false if q.between(ps[i], p)
      next = ps[(j + 1) % ps.length]
      if (next isnt ps[i]) and (next.sub(p).mag() > EPS)
        return false if q.intersects(next, ps[i], p, normal)
    true

class BinaryTreeNode

  constructor: () ->
    @parent = null
    @left = null
    @right = null

  add_children: (l, r) ->
    @left = l
    @right = r
    @left.parent = @
    @right.parent = @

  remove_children: () ->
    @left = null
    @right = null

  has_children: () ->
    @left?

  is_left_child:  () -> @ is @parent?.left
  is_right_child: () -> @ is @parent?.right

  leftmost_descendent: () ->
    out = @; out = out.left while out.has_children(); out

  rightmost_descendent: () ->
    out = @;  out = out.right while out.has_children(); out

  next_child: () ->
    if @has_children()
      return @leftmost_descendent()
    else
      old = @
      old = old.parent while old.is_right_child()
      if not old.parent?
        return old.leftmost_descendent()
      else
        return old.parent.right.leftmost_descendent()

  prev_child: () ->
    if @has_children()
      return @rightmost_descendent()
    else
      old = @
      old = old.parent while old.is_left_child()
      if not old.parent?
        return old.rightmost_descendent()
      else
        return old.parent.left.rightmost_descendent()

  leaves: () ->
    out = [@leftmost_descendent()]
    while out[out.length - 1].next_child() isnt out[0]
      out.push out[out.length - 1].next_child()
    out

  rooted_descendents: () ->
    out = [@]
    front = [@]
    while front.length > 0
      temp = front.pop()
      if temp.has_children()
        out.push temp.left
        out.push temp.right
        front.push temp.left
        front.push temp.right
    out

class Face extends BinaryTreeNode

  @free: 0

  constructor: (@points) ->
    super()
    @id = Face.free++
    @head = @points[0]
    @connections = {}
    @neighbors = {}
    @index = {}
    @connect_points()
    @assign_indices()

  idx:  (p) -> @index[p.id]
  next: (p) -> @connections[p.id]?.next
  prev: (p) -> @connections[p.id]?.prev
  neighbor: (p) -> @neighbors[p.id]
  head_next: () -> @head = @next @head
  head_prev: () -> @head = @prev @head

  normal: () -> Point.normal @points

  points_between: (p, q) ->
    if @next(p)? and @next(q)?
      out = [p]
      while out[out.length - 1] != q
        out.push @next(out[out.length - 1])
      out

  connect_points: () ->
    n = @points.length
    for p,i in @points
      @connections[p.id] = {
        next: @points[(i + 1) % n]
        prev: @points[(i - 1 + n) % n]
      }

  assign_indices: () ->
    n = @points.length
    for p, i in @points
      @index[p.id] = i

  points_in_descendents: () ->
    Face.points_in_faces @rooted_descendents()

  triangle: () -> @points.length is 3

  all_triangles: () ->
    for l in @leaves()
      return false if not l.triangle()
    return true

  @points_in_faces: (fs) ->
    ps = []
    found = {}
    for f in fs
      for p in f.points
        unless found[p.id]?
          found[p.id] = true
          ps.push p
    ps

  @connect_faces: (fs) ->
    for f in fs
      for p in f.points
        f.neighbors[p.id] = null
    for f1, i in fs
      for p1 in f1.points
        for f2 in fs[i+1..]
          for p2 in f2.points
            if (p1 is p2) and
               (f1.next(p1) is f2.prev(p2))
              f1.neighbors[p1.id] = f2
              f2.neighbors[f2.prev(p2).id] = f1

class View
  
  constructor: (@id) ->
    @center = null
    @scale = null
    @dx = new Point 1, 0, 0
    @dy = new Point 0, 1, 0
    @inset_factor = 0.9
    @view_size = 150
    @svg = SVG(@id)
    @svg.viewbox 0, 0, @view_size, @view_size

  clear: () -> @svg.clear()

  scale_points: (ps) ->
    @center = Point.center ps
    @scale = EPS
    @scale = Math.max @scale, @center.sub(p).mag() for p in ps
    @scale = @view_size * @inset_factor / @scale / 2

  set_frame: (dx, dz) ->
    @dx = dx
    dz = @dx.perp() unless dz?
    @dy = dz.cross(@dx)

  location: (p) ->
    q = new Point(
      p.sub(@center).dot(@dx),
      p.sub(@center).dot(@dy)
    )
    q.mul(@scale).neg_y().plus(0.5 * @view_size)

  draw_point: (p) ->
    q = @location p
    @svg.circle(1).center(q.x, q.y)
    
  draw_point_text: (p, label) ->
    q = @location p
    @svg.text(label).move(q.x, q.y)

  draw_polyline: (ps) ->
    qs = (@location p for p in ps)
    @svg.polyline ([q.x, q.y] for q in qs)

  draw_polygon: (ps) ->
    qs = (@location p for p in ps)
    @svg.polygon ([q.x, q.y] for q in qs)

class Rotator

  constructor: (@view, @draw) ->
    @yaw = Math.PI/3
    @pitch = Math.PI/4
    @pitch_lim = 0.42
    @ang_scale = 0.002
    @set_frame @yaw, @pitch
    @start = null
    @current = null

    @view.svg.on 'contextmenu', (e) -> e.preventDefault()
    @view.svg.on 'selectstart', (e) -> e.preventDefault()
    @view.svg.on 'dragstart',   (e) -> e.preventDefault()
    @view.svg.mousedown (e) => @start_spin(e)
    @view.svg.mouseup   (e) => @stop_spin()
    @view.svg.mousemove (e) => @spin(e)
    @view.svg.mouseout  (e) =>
      top = if e.target.id? and e.target.id is @view.id then e.relatedTarget else e.target
      top = top.parentNode while top? and (top.id != @view.id) and (top.nodeName != 'BODY')
      @stop_spin() if not top? or top.nodeName is 'BODY'

  refresh: () -> @set_frame(@yaw, @pitch); @draw()

  set_frame: (yaw, pitch) ->
    dz = new Point(
      Math.cos(pitch) * Math.cos(yaw)
      Math.cos(pitch) * Math.sin(yaw)
      Math.sin(pitch)
    )
    dx = dz.cross(new Point(0, 0, -1)).dir()
    @view.set_frame dx, dz

  stop_spin: () ->
    if @start? and @current?
      @yaw = @get_yaw()
      @pitch = @get_pitch()
      @refresh()
    @start = null
    @current = null

  start_spin: (e) -> @start = @screen_pt(e)

  step_yaw: (d) -> (@yaw + d * @ang_scale * Math.PI) % (2 * Math.PI)

  step_pitch: (d) ->
    pitch = @pitch - d * @ang_scale * Math.PI
    if not (Math.abs(pitch) < @pitch_lim * Math.PI)
      pitch *= @pitch_lim * Math.PI / Math.abs(pitch)
    pitch

  get_yaw:   () -> @step_yaw(@start.x - @current.x)
  get_pitch: () -> @step_pitch(@start.y - @current.y)
  increment_yaw: () -> @yaw = @step_yaw( 10); @refresh()
  decrement_yaw: () -> @yaw = @step_yaw(-10); @refresh()
  increment_pitch: () -> @pitch = @step_pitch( 10); @refresh()
  decrement_pitch: () -> @pitch = @step_pitch(-10); @refresh()

  spin: (e) ->
    if @start?
      @current = @screen_pt(e)
      @set_frame @get_yaw(), @get_pitch()
      @draw()

  screen_pt: (e) ->
    p = @view.svg.node.createSVGPoint()
    p.x = e.clientX
    p.y = e.clientY
    p.matrixTransform @view.svg.node.getScreenCTM().inverse()
    new Point p.x, p.y

###
Hole Specific Code
###

class HolePoint

  @free: 0

  constructor: (@flat, @fold) -> @id = HolePoint.free++

  expansive:   (p) -> @fold.dist_sq(p.fold) - @flat.dist_sq(p.flat) > 10 * EPS
  contractive: (p) -> @flat.dist_sq(p.flat) - @fold.dist_sq(p.fold) > 10 * EPS
  critical:    (p) -> Math.abs(@flat.dist_sq(p.flat) - @fold.dist_sq(p.fold) < 10 * EPS)

  add: (p) -> new HolePoint @flat.add(p.flat), @fold.add(p.fold)
  mul: (s) -> new HolePoint @flat.mul(s), @fold.mul(s)
  sub: (p) -> @add(p.mul(-1))

class Hole extends Face

  CHOICE_N = [60, 20, 20]
  
  constructor: (@points) ->
    super(@points)
    @selected = null
    @space = null
    @choice_mode = 0
    @choice_idx = [0, 0, 0]

  select_next: () ->
    if @selected?
      @selected = @selected.next_child()
      @generate_choices()

  select_prev: () ->
    if @selected?
      @selected = @selected.prev_child()
      @generate_choices()

  select_neighbor: () ->
    if @selected? and @selected.neighbor(@selected.head)?
      @selected = @selected.neighbor(@selected.head)
      @generate_choices()

  visible: (p, q) ->
    Point.visible((v.flat for v in @points),
      @idx(p), q.flat, new Point 0, 0, 1)
  
  has_visible_vertex: (p) -> (q for q in @points when @visible p, q).length > 0

  is_contractive: (p) -> @prev(p).contractive(@next(p))
  
  flat_angle: (p) ->
    Math.abs(p.flat.angle(@prev(p).flat, @next(p).flat) -
      (if p.flat.turns_right(@prev(p).flat, @next(p).flat) then 0 else 2 * Math.PI))

  fold_angle: (p) -> p.fold.angle(@prev(p).fold, @next(p).fold)

  choice: () ->
    if @space?
      if @space[0].length?
        return @space[@choice_idx[0]][@choice_idx[1]][@choice_idx[2]]
      else
        return @space[@choice_idx[0]]
   
  toggle_choice_mode: () ->
    @choice_mode = (@choice_mode + 1) % 3

  choose_next: () ->
    if @space?
      n = (if @space[0].length? then CHOICE_N[@choice_mode] else @space.length)
      i = (if @space[0].length? then @choice_mode else 0)
      @choice_idx[i] = (@choice_idx[i] + 1) % n

  choose_prev: () ->
    if @space?
      n = (if @space[0].length? then CHOICE_N[@choice_mode] else @space.length)
      i = (if @space[0].length? then @choice_mode else 0)
      @choice_idx[i] = (@choice_idx[i] + n - 1) % n

  choose_random: () ->
    if @space?
      if @space[0].length?
        @choice_idx = [Math.floor(Math.random() * CHOICE_N[0]), 0, 0]
      else
        @choice_idx[0] = Math.floor(Math.random() * @space.length)

  get_critical: (p) -> (q for q in @points when @visible(p, q) and p.critical q)
  get_visible:  (p) -> (q for q in @points when @visible(p, q))

  direction_from_angle: (p, a, up) ->
    ang_flat = @flat_angle(p)
    ang_fold = @fold_angle(p)
    if Math.abs(a - ang_flat / 2) <= ang_fold
      b1 = Math.cos(a) + Math.cos(ang_flat - a)
      b2 = Math.cos(a) - Math.cos(ang_flat - a) # not 0 since fold angle not 0
      u = @prev(p).fold.sub(p.fold).dir()
      v = @next(p).fold.sub(p.fold).dir()
      x = u.add(v) # not x.is_zero() since contractive
      y = u.sub(v) # not y.is_zero() since fold angle not 0
      z = x.cross(y).dir()
      x = x.mul(b1 / x.mag_sq())
      y = y.mul(b2 / y.mag_sq())
      zmag = Math.sqrt(1 - x.mag_sq() - y.mag_sq()) # arguement always positive
      z = z.mul up * (if zmag > 0 then zmag else 0)
      q3 = x.add(y).add(z)
      q2 = @prev(p).flat.sub(p.flat).dir().rotate(new Point(0, 0, 1), a)
      new HolePoint(new Point(q2.x, q2.y), new Point(q3.x, q3.y, q3.z))

  get_directions: (p, n) ->
    ang_flat = @flat_angle(p)
    ang_fold = @fold_angle(p)
    if ang_fold > EPS
      n = n / 2
      angles = (i / n * ang_fold + (ang_flat - ang_fold) / 2 for i in [0..n])
      q1 = (@direction_from_angle p, a,  1 for a in angles[0...n])
      q2 = (@direction_from_angle p, a, -1 for a in angles[1..n] by -1)
      return q1.concat q2
    u = @prev(p).sub(p).flat.rotate(new Point(0,0,1), ang_flat / 2).dir()
    v = @next(p).sub(p).fold.dir()
    normal = p.fold.local_normal(@prev(p).fold, @next(p).fold)
    return (new HolePoint(u, v.rotate(normal, ang_flat / 2)
      .rotate(v, i / n * 2 * Math.PI)) for i in [0...n])
    
      

  get_space: (p, [d, x], n) ->
    angle = p.flat.angle(d.flat, @next(p).flat)
    ispace = (angle * i / n for i in [0...n])
    flat_normal = new Point 0, 0, 1
    fold_normal = Point.normal [d.fold, @next(p).fold, p.fold]
    d = d.sub(p)
    (new HolePoint(d.flat.rotate(flat_normal, i),
                   d.fold.rotate(fold_normal, i)) for i in ispace)

  split_from_direction: (p, u) ->
    min = null
    x = null
    for q in @points
      v = q.flat.sub(p.flat)
      vf = q.fold.sub(p.fold)
      denom = u.flat.dot(v) - u.fold.dot(vf)
      if Math.abs(denom) > EPS
        mag = (v.mag_sq() - vf.mag_sq()) / 2 / denom
        if (not min? or min > mag) and @visible(q, u.mul(mag).add(p))
          min = mag
          x = q
    [u.mul(min).add(p), x]

  generate_choices: () ->
    @space = null
    @choice_idx = [0, 0, 0]
    if @selected?
      s = @selected
      cs = s.get_critical s.head
      if cs.length > 0
        @space = cs
      else if s.is_contractive s.head
        n = CHOICE_N[0]
        us = s.get_directions s.head, n
        choices = (s.split_from_direction(s.head, u) for u in us)
        n = CHOICE_N[1]
        us = (s.get_space(s.head, c, n) for c in choices)
        extreme = ((s.split_from_direction(s.head, v) for v in u) for u in us)
        n = CHOICE_N[2]
        @space = (((
          [v[0].sub(s.head).mul(1 - i / n).add(s.head),
          (if i is 0 then v[1] else s.next(s.head))] for i in [0...n]
          ) for v in u) for u in extreme)
    return

  split_critical: (p, q) ->
    @add_children(new Hole(@points_between(p, q)),
                  new Hole(@points_between(q, p)))
    @left

  split_noncritical: (p, [s, q]) ->
    h1 = @points_between(p, q).concat s
    h2 = @points_between(q, p).concat s
    @add_children(new Hole(h1), new Hole(h2))
    @left

  split_next: () ->
    if @selected?
      if @selected.triangle() or @selected.has_children()
        @select_next()
      else
        if @space?
          if @choice().length?
            @selected = @selected.split_noncritical @selected.head, @choice()
          else
            @selected = @selected.split_critical @selected.head, @choice()
        else
          @selected.head_next()
      @generate_choices()

  fully_split_random: () ->
    if @selected?
      @selected.remove_children()
      while true
        @choose_random()
        @split_next()
        break if @all_triangles()

  ###
  Obj Methods
  ###

  get_index: (os) ->
    index = {}
    for o, i in os
      index[o.id] = i unless index[o.id]?
    return index

  v_line: (v) ->
    return "v #{v.fold.x} #{v.fold.y} #{v.fold.z}\n# vf #{v.flat.x} #{v.flat.y}\n"

  f_line: (f, vi, fi) ->
    line = (if f.triangle() then "f" else "# f")
    line += " #{vi[v.id]+1}" for v in f.points
    id = f.id
    parent = (if f.parent? then f.parent.id else f.id)
    left   = (if f.left?   then f.left.id   else f.id)
    right  = (if f.right?  then f.right.id  else f.id)
    line += "\n# fc #{fi[id]} #{fi[parent]} #{fi[left]} #{fi[right]}\n"
    return line

  obj_from_root: () ->
    vs = @points_in_descendents()
    fs = @rooted_descendents()
    vi = @get_index vs
    fi = @get_index fs
    obj = "# This file was created by hole.coffee (c) Jason Ku 2015\n"
    obj += @v_line(v) for v in vs
    obj += "# #{vs.length} vertices\n"
    obj += "# comment vf contains flat vertex location\n"
    obj += @f_line(f, vi, fi) for f in fs
    obj += "# #{fs.length} faces\n"
    obj += "# comment fc contains index/parent/left/right connections\n"
    return obj

  @obj_err: (vs, vfs, fs, fcs) ->
    if vs.length isnt vfs.length
      return "flat and fold vertices not all defined"
    else if fs.length isnt fcs.length
      return "face connections not all defined"
    else
      for v, i in vs  when v.length isnt 3
        return "size of fold vertex #{i} is not 3"
      for v, i in vfs when v.length isnt 2
        return "size of flat vertex #{i} is not 2"
      for f, i in fcs when f.length isnt 4
        return "size of face connection #{i} is not 4"
    return
    
  @root_from_obj: (obj) ->
    lines = obj.split("\n")
    lines[i] = l[2..] for l, i in lines when l.startsWith '# f '
    [vs, vfs, fs, fcs] = [[], [], [], []]
    for l in lines
      vs.push  l[2..].split(' ') if l.startsWith 'v '
      vfs.push l[5..].split(' ') if l.startsWith '# vf '
      fs.push  l[2..].split(' ') if l.startsWith 'f '
      fcs.push l[5..].split(' ') if l.startsWith '# fc '
    err = @obj_err vs, vfs, fs, fcs
    (console.log "Obj Import Error: " + err; return) if err?
    [vs, vfs, fs, fcs] = (((+c for c in l) for l in ls) for ls in [vs, vfs, fs, fcs])
    fs  = ((i - 1 for i in f) for f in fs)
    vs  = (new Point(v[0], v[1], v[2]) for v in vs)
    vfs = (new Point(v[0], v[1]) for v in vfs)
    hvs = (new HolePoint(vfs[i], vs[i]) for i in [0...vs.length])
    hs = []
    for f in fs
      ps = (hvs[i] for i in f)
      return for p in ps when not p?
      hs.push new Hole ps
    for [i, parent, left, right] in fcs
      hs[i].left = hs[left] if left isnt i
      hs[i].right = hs[right] if right isnt i
      hs[i].parent = hs[parent] if parent isnt i
    return h for h in hs when not h.parent?
    return

class HoleView extends View

  constructor: (@id) ->
    super @id
    @show_text = false
    @show_visible = false
    @show_choice = false
    @show_aux = false
    @show_space = false

  toggle_aux: () ->
    if @show_visible
      if not @show_choice
        @show_choice = true
      else if not @show_aux
        @show_aux = true
      else if not @show_space
        @show_space = true
      else
        @show_choice = false
        @show_aux = false
        @show_space = false

  toggle_visible: () ->
    if @show_visible
      @show_choice = false
      @show_visible = false
      @show_aux = false
      @show_space = false
    else
      @show_visible = true

  scale_root: (r) ->
    @scale_points (p[@id] for p in r.points_in_descendents())
    
  draw_hole_point: (p, s) ->
    if p is s.head and @show_visible
      @draw_point(p[@id])
        .fill(YELLOW).stroke(color: RED, linecap: 'round', width: 0.4)
    else if p is s.choice() and @show_choice
      @draw_point(s.choice()[@id])
        .fill(YELLOW).stroke(color: BLUE, linecap: 'round', width: 0.4)
    else
      @draw_point(p[@id]).fill(color: RED, opacity: 0.7).stroke('none')

  draw_hole_point_label: (p, label) ->
    @draw_point_text(p[@id], "#{label}")
      .fill(RED).font(size: '3', anchor: 'end')

  draw_hole_fill: (h, s) ->
    if h.triangle() and ((h isnt s) or not @show_visible)
      @draw_polygon(p[@id] for p in h.points)
        .fill(color: BLUE, opacity: 0.3).stroke('none')
    else if @show_visible and (h is s) and ((@id is 'flat') or h.triangle())
      @draw_polygon(p[@id] for p in h.points)
        .fill(color: YELLOW, opacity: 0.5).stroke('none')

  draw_hole_label: (h, label) ->
    if h.triangle()
      c = Point.centroid(p[@id] for p in h.points)
      @draw_point(c).fill(BLUE).stroke('none')
      @draw_point_text(c, "#{label}")
        .fill(BLUE).font(size: 3, anchor: 'end')

  draw_hole_stroke: (h) ->
    @draw_polygon(p[@id] for p in h.points)
      .fill('none').stroke(color: BLACK, linecap: 'round', width: 0.2, opacity: 0.3)

  draw_selected_edge: (s) ->
    p = s.head
    q = s.next(p)
    @draw_polyline([p[@id],q[@id]])
      .fill('none').stroke(color: RED, linecap: 'round', width: 0.5)
  
  draw_visible_paths: (s) ->
    if @id is 'flat'
      qs = s.get_visible s.head
      for q in qs
        @draw_polyline([s.head[@id],q[@id]])
          .fill('none').stroke(color: GREEN, linecap: 'round', width: 0.2, opacity: 0.7)

  draw_critical_paths: (r) ->
    if r.space? and not r.choice().length?
      for p in r.space when p isnt r.choice()
        @draw_polyline([r.selected.head[@id], p[@id]])
          .fill('none').stroke(color: BLUE, linecap: 'round', width: 0.2, opacity: 0.7)

  draw_directions: (r) ->
    if r.space? and r.choice().length?
      tail = [r.selected.next(r.selected.head), r.selected.head]
      if @show_space
        for sp, i in r.space
          ps = (s for [s,x] in (p[0] for p in sp)).concat tail
          @draw_polygon (s[@id] for s in ps)
            .fill('none').stroke(color: BLACK, linecap: 'round', width: 0.2, opacity: 0.1)
      @draw_polygon (s[@id] for [s, x] in (c[0][0] for c in r.space))
        .fill('none').stroke
          color: (if r.choice_mode is 0 then RED else BLUE)
          linecap: 'round', width: 0.3, opacity: 0.7
      ps = (s for [s,x] in (c[0] for c in r.space[r.choice_idx[0]])).concat tail
      ps.pop()
      @draw_polyline (s[@id] for s in ps)
        .fill('none').stroke
          color: (if r.choice_mode is 1 then RED else BLUE)
          linecap: 'round', width: 0.3, opacity: 0.7
      @draw_polygon (s[@id] for s in [r.space[r.choice_idx[0]][r.choice_idx[1]][0][0],
        r.selected.head])
        .fill('none').stroke
          color: (if r.choice_mode is 2 then RED else BLUE)
          linecap: 'round', width: 0.3, opacity: 0.7

  draw_choice: (r) ->
    if r.space?
      ps = [r.selected.head, r.choice()]
      if r.choice().length?
        ps = [r.selected.head].concat r.choice()[0]
        @draw_polygon (p[@id] for p in ps.concat r.selected.next r.selected.head)
          .fill(color: RED, opacity: 0.5).stroke('none')
        if r.choice_idx[1] is 0
          @draw_polygon (p[@id] for p in ps.concat r.selected.prev r.selected.head)
            .fill(color: RED, opacity: 0.5).stroke('none')
      @draw_polyline (p[@id] for p in ps)
        .fill('none').stroke(color: BLUE, linecap: 'round', width: 0.5, opacity: 1)
      if r.choice().length?
        if r.choice_idx[2] is 0
          @draw_polyline (p[@id] for p in r.choice())
            .fill('none').stroke(color: BLUE, linecap: 'round', width: 0.3, opacity: 1)
        @draw_point(r.choice()[0][@id])
          .fill(YELLOW).stroke(color: BLUE, linecap: 'round', width: 0.4)

  draw_root: (r) ->
    @clear()
    @scale_root(r)
    holes = r.leaves()
    s = r.selected
    @draw_hole_fill h, s for h in holes
    @draw_hole_stroke h for h in holes
    if @show_visible
      @draw_visible_paths s
    if @show_aux
      @draw_critical_paths r
      @draw_directions r
    if @show_choice
      @draw_choice r
    if @show_visible
      @draw_selected_edge s
    ps = r.points_in_descendents()
    @draw_hole_point(p, s) for p in ps
    if @show_text
      @draw_hole_point_label(p, i) for p, i in ps
      @draw_hole_label(h, i) for h, i in holes

class AppHandler

  constructor: () ->
    @root_idx = 0
    @roots = []
    for h in INPUT by -1
      @add_root(new Hole (
        new HolePoint(
          new Point(cs[0], cs[1]),
          new Point(cs[2], cs[3], cs[4])
        ) for cs in h.vertices))

    @view2D = new HoleView 'flat'
    @view3D = new HoleView 'fold'
    @rotator = new Rotator @view3D, () => @view3D.draw_root(@root())
    @draw(); @console_dump()

    $("#KEY_H").click  (e) => @rotator.increment_yaw()
    $("#KEY_J").click  (e) => @rotator.decrement_pitch()
    $("#KEY_K").click  (e) => @rotator.increment_pitch()
    $("#KEY_L").click  (e) => @rotator.decrement_yaw()
    $("#create").click (e) => @write_obj()
    $("#input").change (e) => @read_obj(e)
    
    $("#TAB").click    (e) => @root_next()
    $("#TILDE").click  (e) => @root_prev()
    $("#RIGHT").click  (e) => @head_next()
    $("#LEFT").click   (e) => @head_prev()
    $("#PERIOD").click (e) => @choose_next()
    $("#COMMA").click  (e) => @choose_prev()
    $("#UP").click     (e) => @hole_next()
    $("#DOWN").click   (e) => @hole_prev()
    $("#KEY_A").click  (e) => @toggle_visible()
    $("#KEY_C").click  (e) => @toggle_choice()
    $("#KEY_N").click  (e) => @hole_neighbor()
    $("#KEY_T").click  (e) => @toggle_text()
    $("#KEY_S").click  (e) => @toggle_aux()
    $("#RETURN").click (e) => @split_next()
    $("#DELETE").click (e) => @delete_selected()
    $("#SPACE").click  (e) => @clear_or_full_random()
    
    $(document).on 'keydown', (e) =>
      if [TAB, TILDE, RIGHT, LEFT, UP, DOWN, RETURN, DELETE, SPACE,
          KEY_A, KEY_C, KEY_H, KEY_J, KEY_K, KEY_L,
          KEY_S, KEY_T, KEY_N, PERIOD, COMMA].indexOf(e.which) isnt -1
        e.preventDefault()
      switch e.which
        when TAB    then @root_next()
        when TILDE  then @root_prev()
        when RIGHT  then @head_next()
        when LEFT   then @head_prev()
        when PERIOD then @choose_next()
        when COMMA  then @choose_prev()
        when UP     then @hole_next()
        when DOWN   then @hole_prev()
        when KEY_A  then @toggle_visible()
        when KEY_C  then @toggle_choice()
        when KEY_H  then @rotator.increment_yaw()
        when KEY_J  then @rotator.decrement_pitch()
        when KEY_K  then @rotator.increment_pitch()
        when KEY_L  then @rotator.decrement_yaw()
        when KEY_N  then @hole_neighbor()
        when KEY_T  then @toggle_text()
        when KEY_S  then @toggle_aux()
        when RETURN then @split_next()
        when DELETE then @delete_selected()
        when SPACE  then @clear_or_full_random()

  root: () -> @roots[@root_idx]
  
  selected: () -> @root().selected

  add_root: (r) ->
    @roots.push r
    @root_idx = @roots.length - 1
    @root().selected = @root().next_child()
    @root().generate_choices()
    Face.connect_faces @root().leaves()
    @write_slide_text()

  draw: () ->
    @view2D.draw_root @root()
    @view3D.draw_root @root()

  choose_next: () -> @root().choose_next(); @draw()
  choose_prev: () -> @root().choose_prev(); @draw()
  hole_next:   () -> @root().select_next(); @draw()
  hole_prev:   () -> @root().select_prev(); @draw()
  hole_neighbor: () -> @root().select_neighbor(); @draw()
  root_next: () ->
    @root_idx = (@root_idx + 1) % @roots.length
    @write_slide_text(); @draw()
  root_prev: () ->
    @root_idx = (@root_idx + @roots.length - 1) % @roots.length
    @write_slide_text(); @draw()
  head_next: () -> @selected().head_next(); @root().generate_choices(); @draw()
  head_prev: () -> @selected().head_prev(); @root().generate_choices(); @draw()
  toggle_choice: () -> @root().toggle_choice_mode(); @draw()
  toggle_text: () -> v.show_text = not v.show_text for v in [@view2D, @view3D]; @draw()
  toggle_aux:  () -> v.toggle_aux()  for v in [@view2D, @view3D]; @draw()
  toggle_visible: () -> v.toggle_visible()  for v in [@view2D, @view3D]; @draw()

  write_slide_text: () -> #$("#warnings").text(SLIDE_TEXT[@root_idx])
  console_dump: () -> console.log @

  split_next: () ->
    @root().split_next()
    Face.connect_faces @root().leaves()
    @draw(); @console_dump()

  delete_selected: () ->
    if @selected().parent?
      @root().selected = @selected().parent
      @selected().remove_children()
      Face.connect_faces @root().leaves()
      @root().generate_choices()
      @draw(); @console_dump()

  clear_or_full_random: () ->
    if @root().has_children()
      @root().selected = @root()
      @selected().remove_children()
      @root().generate_choices()
    else
      @root().fully_split_random()
    Face.connect_faces @root().leaves()
    @draw(); @console_dump()

  write_obj: () ->
    data = new Blob [@root().obj_from_root()], {type: 'text/plain'}
    window.URL.revokeObjectURL $("#viewlink")
    $("#viewlink").attr "href", window.URL.createObjectURL data
    $("#viewlink").attr "style", {display: 'block'}

  read_obj: (e) ->
    file_reader = new FileReader()
    file_reader.onload = (e) =>
      h = Hole.root_from_obj e.target.result
      if h?
        @add_root h
        @draw(); @console_dump()
    file_reader.readAsText e.target.files[0]

window?.onload = () ->

  app_handler = new AppHandler()

