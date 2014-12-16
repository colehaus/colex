Elm.Native.Slider = {};
Elm.Native.Slider.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Slider = elm.Native.Slider || {};
    if (elm.Native.Slider.values) return elm.Native.Slider.values;

    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
    var newElement = Elm.Graphics.Element.make(elm).newElement;

    function renderSlider(model) {
        var node = newNode('input');
        node.type = 'range';

        node.min = model.styling.min;
        node.max = model.styling.max;
        node.step = model.styling.step;
        node.value = model.styling.value;

        if (!model.styling.horizontal) {
            node.orient = "vertical"; // FF
            node.style.webkitAppearance = "slider-vertical"; // webkit
            node.style.writingMode = "bt-lr"; // ie
        }

        if (model.styling.disabled) {
            node.disabled = true;
        }

        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        node.addEventListener('input', function() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.value));
        });
        return node;
    }

    function updateSlider(node, oldModel, newModel) {
        if (newModel.styling.disabled) {
            node.disabled = true;
        } else {
            node.disabled = false;
        }
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.min = newModel.styling.min;
        node.max = newModel.styling.max;
        node.step = newModel.styling.step;
        node.value = newModel.styling.value;
    }

    function slider(signal, handler, styling) {
        var width = styling.length;
        var height = 24;
        if (!styling.horizontal) {
            var temp = width;
            width = height;
            height = temp;
        }
        return A3(newElement, width, height, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider,
            update: updateSlider,
            model: { signal:signal, handler:handler, styling:styling }
        });
    }

    return elm.Native.Slider.values = {
        slider:F3(slider)
    };
}
Elm.Automaton = Elm.Automaton || {};
Elm.Automaton.make = function (_elm) {
   "use strict";
   _elm.Automaton = _elm.Automaton || {};
   if (_elm.Automaton.values)
   return _elm.Automaton.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Automaton",
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $List = Elm.List.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Pseudorandom = Elm.Pseudorandom.make(_elm),
   $Render = Elm.Render.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Slider = Elm.Slider.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Ticks = Elm.Ticks.make(_elm),
   $Transition = Elm.Transition.make(_elm),
   $Window = Elm.Window.make(_elm);
   var textStyle = _U.replace([["typeface"
                               ,_L.fromArray(["Open sans"])]
                              ,["color"
                               ,A3($Color.rgb,24,18,30)]],
   $Text.defaultStyle);
   var plainText$ = function ($) {
      return $Text.leftAligned($Text.style(textStyle)($Text.toText($)));
   };
   var stops = A2($List.indexedMap,
   F2(function (i,a) {
      return {ctor: "_Tuple2"
             ,_0: $Basics.toFloat(i) / 3
             ,_1: a};
   }),
   _L.fromArray([0,1,10,100]));
   var legend = F2(function (gs,
   f) {
      return function () {
         var gradient$ = A2($Color.linear,
         {ctor: "_Tuple2"
         ,_0: $Basics.toFloat(gs) / -2
         ,_1: 0},
         {ctor: "_Tuple2"
         ,_0: $Basics.toFloat(gs) / 2
         ,_1: 0})($List.map(A2($Optics$Lens.over,
         $Optics$Lens.snd_,
         function ($) {
            return f($Render.toAlpha($));
         }))(stops));
         return A2($Graphics$Collage.gradient,
         gradient$,
         A2($Graphics$Collage.rect,
         $Basics.toFloat(gs),
         10));
      }();
   });
   var labelSquish = 0.95;
   var labels = function (gs) {
      return $Graphics$Collage.group(A2($List.map,
      function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return $Graphics$Collage.moveX($Basics.toFloat(gs) * _v0._0 * labelSquish)($Graphics$Collage.toForm(plainText$($String.show(_v0._1))));}
            _E.Case($moduleName,
            "between lines 84 and 85");
         }();
      },
      stops));
   };
   var legends = function (gs) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([plainText$("Capital-managed firms")
                   ,A3($Graphics$Collage.collage,
                   gs,
                   35,
                   _L.fromArray([$Graphics$Collage.moveY(15)(A2(legend,
                                gs,
                                $Render.capitalColor))
                                ,$Graphics$Collage.moveX($Basics.toFloat(gs) / -4 * (labelSquish + 1))(labels(gs))
                                ,$Graphics$Collage.moveY(-15)(A2(legend,
                                gs,
                                $Render.laborColor))]))
                   ,plainText$("Labor-managed firms")]));
   };
   var ticks = A2($Ticks.totalTicks,
   $Signal.constant(2.0e-3),
   $Ticks.run.signal);
   var step = F6(function (n,
   n$,
   current,
   base,
   auto,
   al) {
      return !_U.eq(n$,
      n) ? function () {
         var $ = $Transition.autoStep(A2($Optics$Lens._op["@~"],
         $Grid.altruism,
         al)(auto))(current),
         auto$ = $._0,
         next = $._1;
         return {ctor: "_Tuple4"
                ,_0: auto$
                ,_1: next
                ,_2: base
                ,_3: n$};
      }() : {ctor: "_Tuple4"
            ,_0: auto
            ,_1: current
            ,_2: base
            ,_3: n};
   });
   var update = F2(function (s,
   _v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple4":
            return function () {
                 switch (s.ctor)
                 {case "_Tuple3": switch (s._0)
                      {case 0: switch (s._1.ctor)
                           {case "Reseed":
                              return function () {
                                   var $ = $Grid.init(s._2)(_v4._1),
                                   auto$ = $._0,
                                   next = $._1;
                                   return {ctor: "_Tuple4"
                                          ,_0: auto$
                                          ,_1: next
                                          ,_2: _v4._1
                                          ,_3: 0};
                                }();
                              case "Reset":
                              return function () {
                                   var $ = $Grid.init(s._2)(_v4._2),
                                   auto$ = $._0,
                                   next = $._1;
                                   return {ctor: "_Tuple4"
                                          ,_0: auto$
                                          ,_1: next
                                          ,_2: _v4._2
                                          ,_3: 0};
                                }();}
                           break;}
                      switch (s._1.ctor)
                      {case "Pause":
                         return {ctor: "_Tuple4"
                                ,_0: _v4._0
                                ,_1: _v4._1
                                ,_2: _v4._2
                                ,_3: _v4._3};
                         case "Play": return A6(step,
                           _v4._3,
                           s._0,
                           _v4._1,
                           _v4._2,
                           _v4._0,
                           s._2);
                         case "Step": return A6(step,
                           _v4._3,
                           s._0,
                           _v4._1,
                           _v4._2,
                           _v4._0,
                           s._2);}
                      break;}
                 _E.Case($moduleName,
                 "between lines 61 and 68");
              }();}
         _E.Case($moduleName,
         "between lines 61 and 68");
      }();
   });
   var altruism = $Graphics$Input.input(0);
   var altSlider = A3($Slider.slider,
   altruism.handle,
   $Basics.identity,
   $Slider.defaultSlider);
   var gridSize = $Window.width;
   var gridLength = 25;
   var seed = 2348912;
   var initState = function () {
      var $ = $Grid.init(0)(seed),
      auto = $._0,
      seed$ = $._1;
      return {ctor: "_Tuple4"
             ,_0: auto
             ,_1: seed$
             ,_2: seed
             ,_3: 0};
   }();
   var grid = A2($Signal._op["<~"],
   function (_v14) {
      return function () {
         switch (_v14.ctor)
         {case "_Tuple4":
            return _v14._0;}
         _E.Case($moduleName,
         "on line 57, column 26 to 27");
      }();
   },
   A3($Signal.foldp,
   update,
   initState,
   A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["<~"],
   F3(function (v0,v1,v2) {
      return {ctor: "_Tuple3"
             ,_0: v0
             ,_1: v1
             ,_2: v2};
   }),
   ticks),
   $Ticks.run.signal),
   altruism.signal)));
   var main = A2($Signal._op["~"],
   A2($Signal._op["~"],
   A2($Signal._op["<~"],
   F3(function (s,g,gs) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([legends(gs)
                   ,A2($Render.render,gs,g)
                   ,A2($Graphics$Element.spacer,
                   1,
                   5)
                   ,A2($Graphics$Element.flow,
                   $Graphics$Element.right,
                   _L.fromArray([plainText$("Altruism:")
                                ,A2($Graphics$Element.spacer,
                                10,
                                1)
                                ,altSlider
                                ,A2($Graphics$Element.spacer,
                                10,
                                1)
                                ,plainText$("Generation:")
                                ,A2($Graphics$Element.spacer,
                                10,
                                1)
                                ,plainText$($String.show(s))]))
                   ,A2($Graphics$Element.spacer,
                   1,
                   5)
                   ,$Ticks.playControls]));
   }),
   ticks),
   grid),
   gridSize);
   _elm.Automaton.values = {_op: _op
                           ,seed: seed
                           ,gridLength: gridLength
                           ,gridSize: gridSize
                           ,main: main
                           ,initState: initState
                           ,altruism: altruism
                           ,altSlider: altSlider
                           ,grid: grid
                           ,update: update
                           ,step: step
                           ,ticks: ticks
                           ,labelSquish: labelSquish
                           ,labels: labels
                           ,stops: stops
                           ,legend: legend
                           ,legends: legends
                           ,plainText$: plainText$
                           ,textStyle: textStyle};
   return _elm.Automaton.values;
};Elm.Transition = Elm.Transition || {};
Elm.Transition.make = function (_elm) {
   "use strict";
   _elm.Transition = _elm.Transition || {};
   if (_elm.Transition.values)
   return _elm.Transition.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Transition",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Either = Elm.Either.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Optics$Prism = Elm.Optics.Prism.make(_elm),
   $Optics$Traversal = Elm.Optics.Traversal.make(_elm),
   $Pseudorandom = Elm.Pseudorandom.make(_elm),
   $Pseudorandom$Infix = Elm.Pseudorandom.Infix.make(_elm);
   var clean = $Dict.map(A2($Optics$Lens.set,
   $Grid.clean,
   true));
   var sweep = $Dict.filter(F2(function (_v0,
   gp) {
      return function () {
         return $Basics.not(A2($Optics$Lens._op["^@"],
         gp,
         $Grid.clean));
      }();
   }));
   var maximumBy = function (f) {
      return function ($) {
         return $List.last($List.sortBy(f)($Array.toList($)));
      };
   };
   var subCost = F3(function (e,
   c,
   a) {
      return function () {
         switch (e.ctor)
         {case "Left":
            return A2($Optics$Traversal._op["#%"],
              A2($Optics$Traversal._op["@#"],
              $Grid.groups,
              A2($Optics$Traversal._op["##"],
              $Grid.groupAt(e._0),
              A2($Optics$Traversal._op["@#"],
              $Grid.groupType,
              A2($Optics$Traversal._op["?@"],
              $Grid.capitalGroup,
              $Grid.accum)))),
              function (acc) {
                 return acc - c;
              })(a);
            case "Right":
            return A2($Optics$Traversal._op["#%"],
              A2($Optics$Traversal._op["@#"],
              $Grid.grid,
              A2($Optics$Traversal._op["##"],
              $Grid.index(e._0),
              A2($Optics$Traversal._op["?@"],
              $Cell.firm,
              $Cell.accum))),
              function (acc) {
                 return acc - c;
              })(a);}
         _E.Case($moduleName,
         "between lines 99 and 101");
      }();
   });
   var fromMaybe = A2($Basics.flip,
   $Maybe.maybe,
   $Basics.identity);
   var accum = F2(function (f,gs) {
      return function () {
         var _v5 = A2($Optics$Lens._op["^@"],
         f,
         $Cell.firmType);
         switch (_v5.ctor)
         {case "MkCapital":
            return function () {
                 var gs$ = A2($Optics$Traversal._op["#%"],
                 A2($Optics$Traversal._op["##"],
                 $Grid.groupAt(A2($Optics$Lens._op["^@"],
                 f,
                 $Cell.groupId)),
                 A2($Optics$Traversal._op["@#"],
                 $Grid.groupType,
                 A2($Optics$Traversal._op["?@"],
                 $Grid.capitalGroup,
                 $Grid.accum))),
                 F2(function (x,y) {
                    return x + y;
                 })(A2($Optics$Lens._op["^@"],
                 f,
                 $Cell.capitalProfit)))(gs);
                 return {ctor: "_Tuple2"
                        ,_0: A2($Optics$Lens._op["@%"],
                        $Cell.accum,
                        F2(function (x,y) {
                           return x + y;
                        })(A2($Optics$Lens._op["^@"],
                        f,
                        $Cell.laborProfit)))(f)
                        ,_1: gs$};
              }();
            case "MkLabor":
            return {ctor: "_Tuple2"
                   ,_0: A2($Optics$Lens._op["@%"],
                   $Cell.accum,
                   function (a) {
                      return a + A2($Optics$Lens._op["^@"],
                      f,
                      $Cell.laborProfit) + A2($Optics$Lens._op["^@"],
                      f,
                      $Cell.capitalProfit);
                   })(f)
                   ,_1: gs};}
         _E.Case($moduleName,
         "between lines 58 and 63");
      }();
   });
   var maxDiscount = 0.2;
   var claimP = 0.2;
   var claim = F5(function (al,
   gp,
   e,
   i,
   c) {
      return function () {
         var _v7 = A2($Optics$Prism._op["^?"],
         c,
         $Cell.firm);
         switch (_v7.ctor)
         {case "Just":
            return A2($Pseudorandom$Infix._op["<$>"],
              function (r) {
                 return _U.cmp(r,
                 claimP) < 0 ? function () {
                    var _v9 = A2($Optics$Lens._op["^@"],
                    _v7._0,
                    $Cell.firmType);
                    switch (_v9.ctor)
                    {case "MkCapital":
                       return function () {
                            var gi = A2($Optics$Lens._op["^@"],
                            _v7._0,
                            $Cell.groupId);
                            var g = function (_v11) {
                               return function () {
                                  switch (_v11.ctor)
                                  {case "Just": return _v11._0;}
                                  _E.Case($moduleName,
                                  "on line 147, column 37 to 38");
                               }();
                            }(A2($Optics$Traversal._op["!#"],
                            gp,
                            A2($Optics$Traversal._op["##"],
                            $Grid.groupAt(gi),
                            A2($Optics$Traversal._op["@?"],
                            $Grid.groupType,
                            $Grid.capitalGroup))));
                            var acc = A2($Optics$Lens._op["^@"],
                            g,
                            $Grid.accum);
                            return _U.cmp(acc,
                            0) > 0 && _U.cmp(A2($Optics$Lens._op["^@"],
                            e,
                            $Cell.cost),
                            A2($Optics$Lens._op["^@"],
                            g,
                            $Grid.threshold)) < 0 ? $Maybe.Just({ctor: "_Tuple3"
                                                                ,_0: acc
                                                                ,_1: gi
                                                                ,_2: $Maybe.Nothing}) : $Maybe.Nothing;
                         }();
                       case "MkLabor":
                       return function () {
                            var acc = A2($Optics$Lens._op["^@"],
                            _v7._0,
                            $Cell.accum);
                            return _U.cmp(acc,
                            0) > 0 && _U.cmp(A2($Optics$Lens._op["^@"],
                            e,
                            $Cell.cost),
                            A2($Optics$Lens._op["^@"],
                            _v9._0,
                            $Cell.threshold)(al)) < 0 ? $Maybe.Just({ctor: "_Tuple3"
                                                                    ,_0: acc
                                                                    ,_1: A2($Optics$Lens._op["^@"],
                                                                    _v7._0,
                                                                    $Cell.groupId)
                                                                    ,_2: $Maybe.Just(i)}) : $Maybe.Nothing;
                         }();}
                    _E.Case($moduleName,
                    "between lines 138 and 153");
                 }() : $Maybe.Nothing;
              },
              $Pseudorandom.$float);
            case "Nothing":
            return $Pseudorandom.constant($Maybe.Nothing);}
         _E.Case($moduleName,
         "between lines 132 and 153");
      }();
   });
   var grow = F6(function (gd,
   al,
   n,
   i,
   e,
   gp) {
      return A2($Pseudorandom$Infix._op["=<<"],
      function (claimants) {
         return _U.eq(claimants,
         $Array.empty) ? $Pseudorandom.constant($Maybe.Nothing) : function () {
            var $ = A2(maximumBy,
            function (_v14) {
               return function () {
                  switch (_v14.ctor)
                  {case "_Tuple3":
                     return _v14._0;}
                  _E.Case($moduleName,
                  "on line 113, column 53 to 54");
               }();
            },
            claimants),
            a = $._0,
            gi = $._1,
            mi = $._2;
            return function () {
               switch (mi.ctor)
               {case "Just":
                  return A2($Pseudorandom$Infix._op["<$>"],
                    function (r) {
                       return $Maybe.Just({ctor: "_Tuple3"
                                          ,_0: A3($Cell.defLabor,
                                          r * maxDiscount,
                                          n,
                                          gi)
                                          ,_1: A2($Optics$Lens._op["^@"],
                                          e,
                                          $Cell.cost)
                                          ,_2: $Either.Right(mi._0)});
                    },
                    $Pseudorandom.$float);
                  case "Nothing":
                  return $Pseudorandom.constant($Maybe.Just({ctor: "_Tuple3"
                                                            ,_0: A2($Cell.defCapital,
                                                            n,
                                                            gi)
                                                            ,_1: A2($Optics$Lens._op["^@"],
                                                            e,
                                                            $Cell.cost)
                                                            ,_2: $Either.Left(gi)}));}
               _E.Case($moduleName,
               "between lines 114 and 120");
            }();
         }();
      },
      $Pseudorandom.lift($Grid.filterMap($Basics.identity))($Grid.combine($Grid.indexedMap(A3(claim,
      al,
      gp,
      e))(A2($Grid.neighbors,
      gd,
      i)))));
   });
   var deathP = 0.1;
   var death = function () {
      var kill = function (r) {
         return _U.cmp(r,
         deathP) < 0 ? A2($Pseudorandom$Infix._op["<$>"],
         $Maybe.Just,
         $Cell.defEmpty) : $Pseudorandom.constant($Maybe.Nothing);
      };
      return A2($Pseudorandom$Infix._op["=<<"],
      kill,
      $Pseudorandom.$float);
   }();
   var abioP = 1.0e-3;
   var abiogenesis = F3(function (n,
   gi,
   gs) {
      return function () {
         var birth = function (r) {
            return _U.cmp(r,
            abioP) < 0 ? A2($Pseudorandom$Infix._op["<$>"],
            function (r) {
               return $Maybe.Just({ctor: "_Tuple2"
                                  ,_0: A2($Cell.defCapital,n,gi)
                                  ,_1: A2($Optics$Traversal._op["#~"],
                                  $Grid.groupAt(gi),
                                  _L.fromArray([$Grid.defCapital(r * maxDiscount)]))(gs)});
            },
            $Pseudorandom.$float) : _U.cmp(r,
            abioP * 2) < 0 ? A2($Pseudorandom$Infix._op["<$>"],
            function (r) {
               return $Maybe.Just({ctor: "_Tuple2"
                                  ,_0: A3($Cell.defLabor,
                                  r * maxDiscount,
                                  n,
                                  gi)
                                  ,_1: A2($Optics$Traversal._op["#~"],
                                  $Grid.groupAt(gi),
                                  _L.fromArray([$Grid.defLabor]))(gs)});
            },
            $Pseudorandom.$float) : $Pseudorandom.constant($Maybe.Nothing);
         };
         return A2($Pseudorandom$Infix._op["=<<"],
         birth,
         $Pseudorandom.$float);
      }();
   });
   var cellStep = F3(function (init,
   i,
   curr) {
      return function () {
         var _v21 = A2($Optics$Traversal._op["!#"],
         curr,
         A2($Optics$Traversal._op["@#"],
         $Grid.grid,
         $Grid.index(i)));
         switch (_v21.ctor)
         {case "Just":
            switch (_v21._0.ctor)
              {case "MkEmpty":
                 return function () {
                      var grown = function (mf) {
                         return function () {
                            switch (mf.ctor)
                            {case "Just":
                               switch (mf._0.ctor)
                                 {case "_Tuple3":
                                    return $Pseudorandom.constant(A2($Optics$Lens._op["@%"],
                                      $Grid.groups,
                                      $Grid.dirty(mf._0._0))(A2($Optics$Traversal._op["#~"],
                                      A2($Optics$Traversal._op["@#"],
                                      $Grid.grid,
                                      $Grid.index(i)),
                                      _L.fromArray([$Cell.MkFirm(mf._0._0)]))(A3(subCost,
                                      mf._0._2,
                                      mf._0._1,
                                      curr))));}
                                 break;
                               case "Nothing":
                               return function () {
                                    var abio = function (mf) {
                                       return function () {
                                          switch (mf.ctor)
                                          {case "Just":
                                             switch (mf._0.ctor)
                                               {case "_Tuple2":
                                                  return A2($Optics$Lens._op["@~"],
                                                    $Grid.groups,
                                                    mf._0._1)(A2($Optics$Traversal._op["#~"],
                                                    A2($Optics$Traversal._op["@#"],
                                                    $Grid.grid,
                                                    $Grid.index(i)),
                                                    _L.fromArray([$Cell.MkFirm(mf._0._0)]))(curr));}
                                               break;
                                             case "Nothing": return curr;}
                                          _E.Case($moduleName,
                                          "between lines 79 and 84");
                                       }();
                                    };
                                    return A2($Pseudorandom$Infix._op["<$>"],
                                    abio,
                                    A3(abiogenesis,
                                    A2($Optics$Lens._op["^@"],
                                    curr,
                                    $Grid.generation),
                                    A2($Grid.genGroupId,
                                    A2($Optics$Lens._op["^@"],
                                    curr,
                                    $Grid.generation),
                                    i),
                                    A2($Optics$Lens._op["^@"],
                                    curr,
                                    $Grid.groups)));
                                 }();}
                            _E.Case($moduleName,
                            "between lines 73 and 85");
                         }();
                      };
                      return A2($Pseudorandom$Infix._op["=<<"],
                      grown,
                      A6(grow,
                      A2($Optics$Lens._op["^@"],
                      init,
                      $Grid.grid),
                      A2($Optics$Lens._op["^@"],
                      curr,
                      $Grid.altruism),
                      A2($Optics$Lens._op["^@"],
                      curr,
                      $Grid.generation),
                      i,
                      _v21._0._0,
                      A2($Optics$Lens._op["^@"],
                      init,
                      $Grid.groups)));
                   }();
                 case "MkFirm":
                 return function () {
                      var killed = function (me) {
                         return function () {
                            switch (me.ctor)
                            {case "Just":
                               return A2($Optics$Traversal._op["#~"],
                                 A2($Optics$Traversal._op["@#"],
                                 $Grid.grid,
                                 $Grid.index(i)),
                                 _L.fromArray([$Cell.MkEmpty(me._0)]))(curr);
                               case "Nothing":
                               return function () {
                                    var $ = A2(accum,
                                    _v21._0._0,
                                    A2($Optics$Lens._op["^@"],
                                    curr,
                                    $Grid.groups)),
                                    f$ = $._0,
                                    gs$ = $._1;
                                    return A2($Optics$Lens._op["@~"],
                                    $Grid.groups,
                                    A2($Grid.dirty,
                                    f$,
                                    gs$))(A2($Optics$Traversal._op["#~"],
                                    A2($Optics$Traversal._op["@#"],
                                    $Grid.grid,
                                    $Grid.index(i)),
                                    _L.fromArray([$Cell.MkFirm(f$)]))(curr));
                                 }();}
                            _E.Case($moduleName,
                            "between lines 88 and 94");
                         }();
                      };
                      return A2($Pseudorandom$Infix._op["<$>"],
                      killed,
                      death);
                   }();}
              break;}
         _E.Case($moduleName,
         "between lines 70 and 94");
      }();
   });
   var autoStep = function (at) {
      return A2($Pseudorandom$Infix._op["<$>"],
      function ($) {
         return A2($Optics$Lens.over,
         $Grid.groups,
         function ($) {
            return clean(sweep($));
         })(A2($Optics$Lens.over,
         $Grid.generation,
         F2(function (x,y) {
            return x + y;
         })(1))($));
      },
      A3($Grid.foldl,
      F2(function (i,acc) {
         return A2($Pseudorandom$Infix._op["=<<"],
         A2(cellStep,at,i),
         acc);
      }),
      $Pseudorandom.constant(at),
      $Grid.indices));
   };
   _elm.Transition.values = {_op: _op
                            ,abioP: abioP
                            ,deathP: deathP
                            ,claimP: claimP
                            ,maxDiscount: maxDiscount
                            ,abiogenesis: abiogenesis
                            ,death: death
                            ,accum: accum
                            ,fromMaybe: fromMaybe
                            ,cellStep: cellStep
                            ,subCost: subCost
                            ,grow: grow
                            ,maximumBy: maximumBy
                            ,claim: claim
                            ,sweep: sweep
                            ,clean: clean
                            ,autoStep: autoStep};
   return _elm.Transition.values;
};Elm.Render = Elm.Render || {};
Elm.Render.make = function (_elm) {
   "use strict";
   _elm.Render = _elm.Render || {};
   if (_elm.Render.values)
   return _elm.Render.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Render",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Grid = Elm.Grid.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Optics$Prism = Elm.Optics.Prism.make(_elm),
   $Optics$Traversal = Elm.Optics.Traversal.make(_elm);
   var borderStyle = function (c) {
      return function () {
         var s = $Graphics$Collage.solid(c);
         return _U.replace([["width",2]],
         s);
      }();
   };
   var border = F3(function (cs,
   _v0,
   _v1) {
      return function () {
         switch (_v1.ctor)
         {case "_Tuple2":
            return function () {
                 switch (_v0.ctor)
                 {case "_Tuple2":
                    return _U.eq(_v0._1,
                      _v1._1) ? function () {
                         var b = function (x) {
                            return A2($Graphics$Collage.segment,
                            {ctor: "_Tuple2"
                            ,_0: x
                            ,_1: $Basics.toFloat(_v0._1 * cs)},
                            {ctor: "_Tuple2"
                            ,_0: x
                            ,_1: $Basics.toFloat((_v0._1 + 1) * cs)});
                         };
                         var x = $Basics.toFloat(_v0._0 * cs);
                         return {ctor: "_Tuple2"
                                ,_0: b(x + 0.5)
                                ,_1: b(x - 0.5)};
                      }() : _U.eq(_v0._0,
                      _v1._0) ? function () {
                         var b = function (y) {
                            return A2($Graphics$Collage.segment,
                            {ctor: "_Tuple2"
                            ,_0: $Basics.toFloat(_v0._0 * cs)
                            ,_1: y},
                            {ctor: "_Tuple2"
                            ,_0: $Basics.toFloat((_v0._0 + 1) * cs)
                            ,_1: y});
                         };
                         var y = $Basics.toFloat(_v0._1 * cs);
                         return {ctor: "_Tuple2"
                                ,_0: b(y + 0.5)
                                ,_1: b(y - 0.5)};
                      }() : _E.If($moduleName,
                      "between lines 63 and 70");}
                 _E.Case($moduleName,
                 "between lines 63 and 70");
              }();}
         _E.Case($moduleName,
         "between lines 63 and 70");
      }();
   });
   var cellSize = function (gs) {
      return $Basics.floor($Basics.toFloat(gs) / $Basics.toFloat($Grid.length));
   };
   var botLeft = function (s) {
      return $Graphics$Collage.move({ctor: "_Tuple2"
                                    ,_0: $Basics.toFloat(s) / -2
                                    ,_1: $Basics.toFloat(s) / -2});
   };
   var toAlpha = function (x) {
      return 1 - 1 / A2($Basics.logBase,
      $Basics.e,
      $Basics.e + A2($Basics.max,
      x,
      0)) * 0.8;
   };
   var both = F2(function (f,_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: f(_v8._0)
                   ,_1: f(_v8._1)};}
         _E.Case($moduleName,
         "on line 35, column 18 to 26");
      }();
   });
   var positionCell = F2(function (gs,
   cs) {
      return function ($) {
         return $Graphics$Collage.move(both(function (i) {
            return $Basics.toFloat(i) * $Basics.toFloat(cs) + $Basics.toFloat(cs) / 2;
         })($));
      };
   });
   var emptyColor = A3($Color.rgba,
   156,
   69,
   53);
   var laborAt0 = A3($Color.rgb,
   215,
   233,
   215);
   var laborColor = A3($Color.rgba,
   57,
   147,
   57);
   var capitalColor = A3($Color.rgba,
   116,
   46,
   153);
   var color = function (c) {
      return function () {
         switch (c.ctor)
         {case "MkEmpty":
            return emptyColor(toAlpha(A2($Optics$Lens._op["^@"],
              c._0,
              $Cell.cost)));
            case "MkFirm":
            return function () {
                 var _v15 = A2($Optics$Lens._op["^@"],
                 c._0,
                 $Cell.firmType);
                 switch (_v15.ctor)
                 {case "MkCapital":
                    return capitalColor(toAlpha(A2($Optics$Lens._op["^@"],
                      c._0,
                      $Cell.accum)));
                    case "MkLabor":
                    return laborColor(toAlpha(A2($Optics$Lens._op["^@"],
                      c._0,
                      $Cell.accum)));}
                 _E.Case($moduleName,
                 "between lines 53 and 55");
              }();}
         _E.Case($moduleName,
         "between lines 50 and 55");
      }();
   };
   var cells = F2(function (gs,g) {
      return function () {
         var cs = cellSize(gs);
         var draw = F2(function (i,
         cell) {
            return A3(positionCell,
            gs,
            cs,
            i)($Graphics$Collage.filled(color(cell))($Graphics$Collage.square($Basics.toFloat(cs))));
         });
         return $Graphics$Collage.group($Grid.toList(A2($Grid.indexedMap,
         draw,
         g)));
      }();
   });
   var lineOfBorders = F3(function (cs,
   gs,
   a) {
      return function () {
         var border$ = F3(function (_v17,
         _v18,
         fs) {
            return function () {
               switch (_v18.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v17.ctor)
                       {case "_Tuple2":
                          return !_U.eq(A2($Optics$Traversal._op["!#"],
                            _v17._1,
                            A2($Optics$Traversal._op["?@"],
                            $Cell.firm,
                            $Cell.groupId)),
                            A2($Optics$Traversal._op["!#"],
                            _v18._1,
                            A2($Optics$Traversal._op["?@"],
                            $Cell.firm,
                            $Cell.groupId))) ? function () {
                               var go = F2(function (p,c) {
                                  return $Maybe.map(function (f) {
                                     return function () {
                                        var _v25 = A2($Optics$Lens._op["^@"],
                                        f,
                                        $Cell.firmType);
                                        switch (_v25.ctor)
                                        {case "MkCapital":
                                           return function () {
                                                var a = function (_v27) {
                                                   return function () {
                                                      switch (_v27.ctor)
                                                      {case "Just":
                                                         return _v27._0;}
                                                      _E.Case($moduleName,
                                                      "on line 87, column 43 to 44");
                                                   }();
                                                }(A2($Optics$Traversal._op["!#"],
                                                A2($Dict.getOrFail,
                                                A2($Optics$Lens._op["^@"],
                                                f,
                                                $Cell.groupId),
                                                gs),
                                                A2($Optics$Traversal._op["@#"],
                                                $Grid.groupType,
                                                A2($Optics$Traversal._op["?@"],
                                                $Grid.capitalGroup,
                                                $Grid.accum))));
                                                return $Graphics$Collage.group(_L.fromArray([A2($Graphics$Collage.traced,
                                                                                            borderStyle($Color.white),
                                                                                            p)
                                                                                            ,A2($Graphics$Collage.traced,
                                                                                            borderStyle(capitalColor(toAlpha(a))),
                                                                                            p)]));
                                             }();
                                           case "MkLabor":
                                           return A2($Graphics$Collage.traced,
                                             borderStyle(laborAt0),
                                             p);}
                                        _E.Case($moduleName,
                                        "between lines 85 and 94");
                                     }();
                                  })(A2($Optics$Prism._op["^?"],
                                  c,
                                  $Cell.firm));
                               });
                               var $ = A3(border,
                               cs,
                               _v17._0,
                               _v18._0),
                               p = $._0,
                               p$ = $._1;
                               return $Array.append(fs)($Array.fromList(A2($List.filterMap,
                               $Basics.identity,
                               _L.fromArray([A2(go,p,_v17._1)
                                            ,A2(go,p$,_v18._1)]))));
                            }() : fs;}
                       _E.Case($moduleName,
                       "between lines 80 and 97");
                    }();}
               _E.Case($moduleName,
               "between lines 80 and 97");
            }();
         });
         return _U.eq(a,
         $Array.empty) ? $Array.empty : $Basics.snd(A3($Array.foldl,
         F2(function (a,_v30) {
            return function () {
               switch (_v30.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: a
                         ,_1: A3(border$,
                         a,
                         _v30._0,
                         _v30._1)};}
               _E.Case($moduleName,
               "on line 100, column 40 to 58");
            }();
         }),
         {ctor: "_Tuple2"
         ,_0: A2($Array.getOrFail,0,a)
         ,_1: $Array.empty},
         A3($Array.slice,
         1,
         $Array.length(a),
         a)));
      }();
   });
   var borders = F2(function (gs,
   a) {
      return function () {
         var cs = cellSize(gs);
         var go = function (f) {
            return A3(f,
            function ($) {
               return $Array.append(A2(lineOfBorders,
               cs,
               A2($Optics$Lens._op["^@"],
               a,
               $Grid.groups))($));
            },
            $Array.empty,
            A2($Optics$Lens._op["^@"],
            a,
            $Grid.grid));
         };
         return $Graphics$Collage.group($Array.toList(A2($Array.append,
         go($Grid.indexedFoldFromBottom),
         go($Grid.indexedFoldFromLeft))));
      }();
   });
   var render = F2(function (gs,
   g) {
      return A3($Graphics$Collage.collage,
      gs,
      gs,
      _L.fromArray([botLeft(gs)(A2(cells,
                   gs,
                   A2($Optics$Lens._op["^@"],
                   g,
                   $Grid.grid)))
                   ,botLeft(gs)(A2(borders,
                   gs,
                   g))]));
   });
   _elm.Render.values = {_op: _op
                        ,capitalColor: capitalColor
                        ,laborColor: laborColor
                        ,laborAt0: laborAt0
                        ,emptyColor: emptyColor
                        ,both: both
                        ,toAlpha: toAlpha
                        ,botLeft: botLeft
                        ,positionCell: positionCell
                        ,color: color
                        ,cellSize: cellSize
                        ,border: border
                        ,borderStyle: borderStyle
                        ,lineOfBorders: lineOfBorders
                        ,borders: borders
                        ,cells: cells
                        ,render: render};
   return _elm.Render.values;
};Elm.Grid = Elm.Grid || {};
Elm.Grid.make = function (_elm) {
   "use strict";
   _elm.Grid = _elm.Grid || {};
   if (_elm.Grid.values)
   return _elm.Grid.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Grid",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Cell = Elm.Cell.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Either = Elm.Either.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Optics$Prism = Elm.Optics.Prism.make(_elm),
   $Optics$Traversal = Elm.Optics.Traversal.make(_elm),
   $Pseudorandom = Elm.Pseudorandom.make(_elm),
   $Pseudorandom$Infix = Elm.Pseudorandom.Infix.make(_elm);
   var foldl = $Array.foldl;
   var filter = $Array.filter;
   var empty = $Array.empty;
   var map = $Array.map;
   var combine = $Pseudorandom.combineA;
   var mapR = $Pseudorandom.mapA;
   var fromList = $Array.fromList;
   var foldr = $Array.foldr;
   var toList = $Array.toList;
   var maybeToList = A2($Maybe.maybe,
   _L.fromArray([]),
   function (a) {
      return _L.fromArray([a]);
   });
   var reverse = A2($Array.foldr,
   $Array.push,
   $Array.empty);
   var map2 = F3(function (f,
   xs,
   ys) {
      return function () {
         var go = F2(function (xs,
         ys) {
            return function () {
               var _v0 = {ctor: "_Tuple2"
                         ,_0: A2($Array.get,0,xs)
                         ,_1: A2($Array.get,0,ys)};
               switch (_v0.ctor)
               {case "_Tuple2":
                  switch (_v0._0.ctor)
                    {case "Just":
                       switch (_v0._1.ctor)
                         {case "Just":
                            return A2($Array.push,
                              A2(f,_v0._0._0,_v0._1._0),
                              A2(go,
                              A3($Array.slice,
                              1,
                              $Array.length(xs),
                              xs),
                              A3($Array.slice,
                              1,
                              $Array.length(ys),
                              ys)));}
                         break;}
                    break;}
               return $Array.empty;
            }();
         });
         return reverse(A2(go,xs,ys));
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v5 = f(mx);
         switch (_v5.ctor)
         {case "Just":
            return A2($Array.push,
              _v5._0,
              xs);
            case "Nothing": return xs;}
         _E.Case($moduleName,
         "between lines 137 and 139");
      }();
   });
   var filterMap = function (f) {
      return A2($Array.foldl,
      maybeCons(f),
      $Array.empty);
   };
   var groupType = A2($Optics$Lens.Lens,
   function (_) {
      return _.groupType;
   },
   F2(function (a,s) {
      return _U.replace([["groupType"
                         ,a]],
      s);
   }));
   var clean = A2($Optics$Lens.Lens,
   function (_) {
      return _.clean;
   },
   F2(function (a,s) {
      return _U.replace([["clean"
                         ,a]],
      s);
   }));
   var dirty = function (f) {
      return A2($Dict.update,
      A2($Optics$Lens._op["^@"],
      f,
      $Cell.groupId),
      $Maybe.map(A2($Optics$Lens.set,
      clean,
      false)));
   };
   var threshold = A2($Optics$Lens.Lens,
   function (_) {
      return _.threshold;
   },
   F2(function (a,s) {
      return _U.replace([["threshold"
                         ,a]],
      s);
   }));
   var accum = A2($Optics$Lens.Lens,
   function (_) {
      return _.accum;
   },
   F2(function (a,s) {
      return _U.replace([["accum"
                         ,a]],
      s);
   }));
   var CapitalGroup = F2(function (a,
   b) {
      return {_: {}
             ,accum: a
             ,threshold: b};
   });
   var MkLabor = {ctor: "MkLabor"};
   var MkCapital = function (a) {
      return {ctor: "MkCapital"
             ,_0: a};
   };
   var capitalGroup = A2($Optics$Prism.Prism,
   MkCapital,
   function (s) {
      return function () {
         switch (s.ctor)
         {case "MkCapital":
            return $Either.Right(s._0);}
         return $Either.Left(s);
      }();
   });
   var Group = F2(function (a,b) {
      return {_: {}
             ,clean: a
             ,groupType: b};
   });
   var defCapital = function (d) {
      return Group(false)(MkCapital(CapitalGroup(0)($Cell.capitalShare * $Cell.stepProfit / (1 - (1 - $Cell.deathP) * (1 - d)))));
   };
   var defLabor = A2(Group,
   false,
   MkLabor);
   var groups = A2($Optics$Lens.Lens,
   function (_) {
      return _.groups;
   },
   F2(function (a,s) {
      return _U.replace([["groups"
                         ,a]],
      s);
   }));
   var grid = A2($Optics$Lens.Lens,
   function (_) {
      return _.grid;
   },
   F2(function (a,s) {
      return _U.replace([["grid"
                         ,a]],
      s);
   }));
   var altruism = A2($Optics$Lens.Lens,
   function (_) {
      return _.altruism;
   },
   F2(function (a,s) {
      return _U.replace([["altruism"
                         ,a]],
      s);
   }));
   var generation = A2($Optics$Lens.Lens,
   function (_) {
      return _.generation;
   },
   F2(function (a,s) {
      return _U.replace([["generation"
                         ,a]],
      s);
   }));
   var groupAt = function (k) {
      return A2($Optics$Traversal.Traversal,
      function ($) {
         return maybeToList($Dict.get(k)($));
      },
      F2(function (bs,s) {
         return function () {
            switch (bs.ctor)
            {case "::": switch (bs._1.ctor)
                 {case "[]":
                    return A3($Dict.insert,
                      k,
                      bs._0,
                      s);}
                 break;
               case "[]":
               return A2($Dict.remove,k,s);}
            _E.Case($moduleName,
            "between lines 44 and 46");
         }();
      }));
   };
   var Automaton = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,altruism: d
             ,generation: c
             ,grid: a
             ,groups: b};
   });
   var length = 25;
   var init = function (a) {
      return A2($Pseudorandom$Infix._op["<$>"],
      function (gd) {
         return A4(Automaton,
         gd,
         $Dict.empty,
         1,
         a);
      },
      $Pseudorandom.combineA(A2($Array.repeat,
      Math.pow(length,2),
      A2($Pseudorandom$Infix._op["<$>"],
      $Cell.MkEmpty,
      $Cell.defEmpty))));
   };
   var foldFromLeft = F3(function (f,
   acc,
   g) {
      return function () {
         var gl = $Array.length(g);
         return _U.eq(gl,length) ? A2(f,
         g,
         acc) : A3(foldFromLeft,
         f,
         A2(f,
         A3($Array.slice,0,length,g),
         acc),
         A3($Array.slice,
         length,
         $Array.length(g),
         g));
      }();
   });
   var foldFromBottom = F3(function (f,
   acc,
   g) {
      return function () {
         var go = F2(function (r,
         acc) {
            return _U.eq(r,
            length) ? acc : function () {
               var row = A2($Array.initialize,
               length,
               function (c) {
                  return A2($Array.getOrFail,
                  c * length + r,
                  g);
               });
               return go(r + 1)(A2(f,row,acc));
            }();
         });
         return A2(go,0,acc);
      }();
   });
   var index = function (_v12) {
      return function () {
         switch (_v12.ctor)
         {case "_Tuple2":
            return A2($Optics$Traversal.Traversal,
              function ($) {
                 return maybeToList($Array.get(_v12._0 * length + _v12._1)($));
              },
              function (_v16) {
                 return function () {
                    switch (_v16.ctor)
                    {case "::":
                       switch (_v16._1.ctor)
                         {case "[]":
                            return A2($Array.set,
                              _v12._0 * length + _v12._1,
                              _v16._0);}
                         break;}
                    _E.Case($moduleName,
                    "on line 143, column 39 to 67");
                 }();
              });}
         _E.Case($moduleName,
         "between lines 142 and 143");
      }();
   };
   var indices = A2($Array.initialize,
   Math.pow(length,2),
   function (i) {
      return {ctor: "_Tuple2"
             ,_0: i / length | 0
             ,_1: A2($Basics.rem,i,length)};
   });
   var indexedMap = function (f) {
      return A2(map2,f,indices);
   };
   var indexedFilter = function (f) {
      return function ($) {
         return A2($Array.foldl,
         F2(function (_v20,acc) {
            return function () {
               switch (_v20.ctor)
               {case "_Tuple2": return A2(f,
                    _v20._0,
                    _v20._1) ? A2($Array.push,
                    _v20._1,
                    acc) : acc;}
               _E.Case($moduleName,
               "between lines 95 and 97");
            }();
         }),
         $Array.empty)(indexedMap(F2(function (v0,
         v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }))($));
      };
   };
   var indexedFoldFromLeft = F2(function (f,
   acc) {
      return function ($) {
         return A2(foldFromLeft,
         f,
         acc)(indexedMap(F2(function (v0,
         v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }))($));
      };
   });
   var indexedFoldFromBottom = F2(function (f,
   acc) {
      return function ($) {
         return A2(foldFromBottom,
         f,
         acc)(indexedMap(F2(function (v0,
         v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }))($));
      };
   });
   var indexedFoldl = F3(function (f,
   acc,
   xs) {
      return A2($Array.foldl,
      F2(function (_v24,acc) {
         return function () {
            switch (_v24.ctor)
            {case "_Tuple2": return A3(f,
                 _v24._0,
                 _v24._1,
                 acc);}
            _E.Case($moduleName,
            "on line 126, column 49 to 58");
         }();
      }),
      acc)(A2(indexedMap,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      xs));
   });
   var neighbors = F2(function (g,
   _v28) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple2":
            return function () {
                 var inBounds = function (n) {
                    return _U.cmp(n,
                    0) > -1 && _U.cmp(n,length) < 0;
                 };
                 return filterMap(function (_v32) {
                    return function () {
                       switch (_v32.ctor)
                       {case "_Tuple2":
                          return inBounds(_v32._0) && inBounds(_v32._1) ? A2($Optics$Traversal._op["!#"],
                            g,
                            index({ctor: "_Tuple2"
                                  ,_0: _v32._0
                                  ,_1: _v32._1})) : $Maybe.Nothing;}
                       _E.Case($moduleName,
                       "between lines 164 and 166");
                    }();
                 })($Array.fromList(_L.fromArray([{ctor: "_Tuple2"
                                                  ,_0: _v28._0 + 1
                                                  ,_1: _v28._1}
                                                 ,{ctor: "_Tuple2"
                                                  ,_0: _v28._0 - 1
                                                  ,_1: _v28._1}
                                                 ,{ctor: "_Tuple2"
                                                  ,_0: _v28._0
                                                  ,_1: _v28._1 + 1}
                                                 ,{ctor: "_Tuple2"
                                                  ,_0: _v28._0
                                                  ,_1: _v28._1 - 1}])));
              }();}
         _E.Case($moduleName,
         "between lines 162 and 171");
      }();
   });
   var genGroupId = F2(function (n,
   _v36) {
      return function () {
         switch (_v36.ctor)
         {case "_Tuple2":
            return (_v36._0 * length + _v36._1) * n;}
         _E.Case($moduleName,
         "on line 174, column 24 to 43");
      }();
   });
   _elm.Grid.values = {_op: _op
                      ,length: length
                      ,init: init
                      ,Automaton: Automaton
                      ,groupAt: groupAt
                      ,generation: generation
                      ,altruism: altruism
                      ,grid: grid
                      ,groups: groups
                      ,Group: Group
                      ,MkCapital: MkCapital
                      ,MkLabor: MkLabor
                      ,CapitalGroup: CapitalGroup
                      ,defCapital: defCapital
                      ,defLabor: defLabor
                      ,capitalGroup: capitalGroup
                      ,accum: accum
                      ,threshold: threshold
                      ,clean: clean
                      ,groupType: groupType
                      ,indexedFilter: indexedFilter
                      ,foldFromLeft: foldFromLeft
                      ,foldFromBottom: foldFromBottom
                      ,indexedFoldFromLeft: indexedFoldFromLeft
                      ,indexedFoldFromBottom: indexedFoldFromBottom
                      ,indexedFoldl: indexedFoldl
                      ,indexedMap: indexedMap
                      ,filterMap: filterMap
                      ,maybeCons: maybeCons
                      ,index: index
                      ,reverse: reverse
                      ,map2: map2
                      ,indices: indices
                      ,neighbors: neighbors
                      ,genGroupId: genGroupId
                      ,maybeToList: maybeToList
                      ,dirty: dirty
                      ,toList: toList
                      ,foldr: foldr
                      ,fromList: fromList
                      ,mapR: mapR
                      ,combine: combine
                      ,map: map
                      ,empty: empty
                      ,filter: filter
                      ,foldl: foldl};
   return _elm.Grid.values;
};Elm.Cell = Elm.Cell || {};
Elm.Cell.make = function (_elm) {
   "use strict";
   _elm.Cell = _elm.Cell || {};
   if (_elm.Cell.values)
   return _elm.Cell.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Cell",
   $Basics = Elm.Basics.make(_elm),
   $Either = Elm.Either.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Optics$Prism = Elm.Optics.Prism.make(_elm),
   $Pseudorandom = Elm.Pseudorandom.make(_elm),
   $Pseudorandom$Infix = Elm.Pseudorandom.Infix.make(_elm);
   var threshold = A2($Optics$Lens.Lens,
   function (_) {
      return _.threshold;
   },
   F2(function (a,s) {
      return _U.replace([["threshold"
                         ,a]],
      s);
   }));
   var Labor = function (a) {
      return {_: {},threshold: a};
   };
   var MkCapital = {ctor: "MkCapital"};
   var MkLabor = function (a) {
      return {ctor: "MkLabor"
             ,_0: a};
   };
   var labor = A2($Optics$Prism.Prism,
   MkLabor,
   function (s) {
      return function () {
         switch (s.ctor)
         {case "MkLabor":
            return $Either.Right(s._0);}
         return $Either.Left(s);
      }();
   });
   var firmType = A2($Optics$Lens.Lens,
   function (_) {
      return _.firmType;
   },
   F2(function (a,s) {
      return _U.replace([["firmType"
                         ,a]],
      s);
   }));
   var birthDate = A2($Optics$Lens.Lens,
   function (_) {
      return _.birthDate;
   },
   F2(function (a,s) {
      return _U.replace([["birthDate"
                         ,a]],
      s);
   }));
   var groupId = A2($Optics$Lens.Lens,
   function (_) {
      return _.groupId;
   },
   F2(function (a,s) {
      return _U.replace([["groupId"
                         ,a]],
      s);
   }));
   var capitalProfit = A2($Optics$Lens.Lens,
   function (_) {
      return _.capitalProfit;
   },
   F2(function (a,s) {
      return _U.replace([["capitalProfit"
                         ,a]],
      s);
   }));
   var laborProfit = A2($Optics$Lens.Lens,
   function (_) {
      return _.laborProfit;
   },
   F2(function (a,s) {
      return _U.replace([["laborProfit"
                         ,a]],
      s);
   }));
   var accum = A2($Optics$Lens.Lens,
   function (_) {
      return _.accum;
   },
   F2(function (a,s) {
      return _U.replace([["accum"
                         ,a]],
      s);
   }));
   var Firm = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,accum: c
             ,birthDate: e
             ,capitalProfit: b
             ,firmType: f
             ,groupId: d
             ,laborProfit: a};
   });
   var cost = A2($Optics$Lens.Lens,
   function (_) {
      return _.cost;
   },
   F2(function (a,s) {
      return _U.replace([["cost"
                         ,a]],
      s);
   }));
   var Empty = function (a) {
      return {_: {},cost: a};
   };
   var MkEmpty = function (a) {
      return {ctor: "MkEmpty"
             ,_0: a};
   };
   var emptyCell = A2($Optics$Prism.Prism,
   MkEmpty,
   function (s) {
      return function () {
         switch (s.ctor)
         {case "MkEmpty":
            return $Either.Right(s._0);}
         return $Either.Left(s);
      }();
   });
   var MkFirm = function (a) {
      return {ctor: "MkFirm"
             ,_0: a};
   };
   var firm = A2($Optics$Prism.Prism,
   MkFirm,
   function (s) {
      return function () {
         switch (s.ctor)
         {case "MkFirm":
            return $Either.Right(s._0);}
         return $Either.Left(s);
      }();
   });
   var maxCost = 0.3;
   var defEmpty = A2($Pseudorandom$Infix._op["<$>"],
   function (r) {
      return Empty(r * maxCost);
   },
   $Pseudorandom.$float);
   var deathP = 0.1;
   var laborAdvantage = 1.1;
   var stepProfit = 0.1;
   var capitalShare = 0.3;
   var laborShare = 0.7;
   var defLabor = F3(function (d,
   n,
   i) {
      return function () {
         var stepAd = stepProfit * laborAdvantage - laborShare * stepProfit;
         var calcThreshold = F2(function (discount,
         altruism) {
            return stepAd * altruism / (1 - (1 - discount) * (1 - deathP));
         });
         return A6(Firm,
         stepProfit * laborShare * laborAdvantage,
         stepProfit * capitalShare * laborAdvantage,
         0,
         i,
         n,
         MkLabor(Labor(calcThreshold(d))));
      }();
   });
   var defCapital = F2(function (n,
   i) {
      return A6(Firm,
      stepProfit * laborShare,
      stepProfit * capitalShare,
      0,
      i,
      n,
      MkCapital);
   });
   _elm.Cell.values = {_op: _op
                      ,laborShare: laborShare
                      ,capitalShare: capitalShare
                      ,stepProfit: stepProfit
                      ,laborAdvantage: laborAdvantage
                      ,deathP: deathP
                      ,maxCost: maxCost
                      ,MkFirm: MkFirm
                      ,MkEmpty: MkEmpty
                      ,firm: firm
                      ,emptyCell: emptyCell
                      ,Empty: Empty
                      ,cost: cost
                      ,Firm: Firm
                      ,accum: accum
                      ,laborProfit: laborProfit
                      ,capitalProfit: capitalProfit
                      ,groupId: groupId
                      ,birthDate: birthDate
                      ,firmType: firmType
                      ,MkLabor: MkLabor
                      ,MkCapital: MkCapital
                      ,labor: labor
                      ,Labor: Labor
                      ,threshold: threshold
                      ,defLabor: defLabor
                      ,defCapital: defCapital
                      ,defEmpty: defEmpty};
   return _elm.Cell.values;
};Elm.Optics = Elm.Optics || {};
Elm.Optics.Traversal = Elm.Optics.Traversal || {};
Elm.Optics.Traversal.make = function (_elm) {
   "use strict";
   _elm.Optics = _elm.Optics || {};
   _elm.Optics.Traversal = _elm.Optics.Traversal || {};
   if (_elm.Optics.Traversal.values)
   return _elm.Optics.Traversal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Optics.Traversal",
   $Basics = Elm.Basics.make(_elm),
   $Either = Elm.Either.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Optics$Lens = Elm.Optics.Lens.make(_elm),
   $Optics$Prism = Elm.Optics.Prism.make(_elm);
   var single = function (a) {
      return _L.fromArray([a]);
   };
   var maybeToList = A2($Maybe.maybe,
   _L.fromArray([]),
   single);
   var set = function (_) {
      return _.set;
   };
   _op["#~"] = set;
   var view = function (_) {
      return _.view;
   };
   _op["^#"] = $Basics.flip(view);
   var preview = function (tr) {
      return function ($) {
         return function (la) {
            return function () {
               switch (la.ctor)
               {case "::":
                  return $Maybe.Just(la._0);
                  case "[]":
                  return $Maybe.Nothing;}
               _E.Case($moduleName,
               "between lines 28 and 30");
            }();
         }(view(tr)($));
      };
   };
   _op["!#"] = $Basics.flip(preview);
   var over = F3(function (t,f,s) {
      return A3(set,
      t,
      $List.map(f)(A2(view,t,s)),
      s);
   });
   _op["#%"] = over;
   var Traversal = F2(function (a,
   b) {
      return {_: {}
             ,set: b
             ,view: a};
   });
   _op["##"] = F2(function (p,q) {
      return A2(Traversal,
      function ($) {
         return $List.concatMap(view(q))(view(p)($));
      },
      function (bs) {
         return A2(over,
         p,
         function (x) {
            return A3(set,q,bs,x);
         });
      });
   });
   var pToT = function (pr) {
      return A2(Traversal,
      function ($) {
         return maybeToList($Optics$Prism.preview(pr)($));
      },
      F2(function (bs,s) {
         return function () {
            var _v3 = pr.matching(s);
            switch (_v3.ctor)
            {case "Left": return _v3._0;
               case "Right":
               return A3($Optics$Prism.set,
                 pr,
                 $List.head(bs),
                 s);}
            _E.Case($moduleName,
            "between lines 60 and 62");
         }();
      }));
   };
   _op["#?"] = F2(function (t,p) {
      return A2(_op["##"],
      t,
      pToT(p));
   });
   _op["?#"] = F2(function (p,t) {
      return A2(_op["##"],
      pToT(p),
      t);
   });
   _op["?#?"] = F2(function (p,q) {
      return A2(_op["##"],
      pToT(p),
      pToT(q));
   });
   var lToT = function (ln) {
      return A2(Traversal,
      function ($) {
         return single($Optics$Lens.view(ln)($));
      },
      function (_v6) {
         return function () {
            switch (_v6.ctor)
            {case "::": switch (_v6._1.ctor)
                 {case "[]":
                    return A2($Optics$Lens.set,
                      ln,
                      _v6._0);}
                 break;}
            _E.Case($moduleName,
            "on line 66, column 30 to 40");
         }();
      });
   };
   _op["#@"] = F2(function (t,l) {
      return A2(_op["##"],
      t,
      lToT(l));
   });
   _op["@#"] = F2(function (l,t) {
      return A2(_op["##"],
      lToT(l),
      t);
   });
   _op["?@"] = F2(function (p,l) {
      return A2(_op["##"],
      pToT(p),
      lToT(l));
   });
   _op["@?"] = F2(function (l,p) {
      return A2(_op["##"],
      lToT(l),
      pToT(p));
   });
   _op["@#@"] = F2(function (l,m) {
      return A2(_op["##"],
      lToT(l),
      lToT(m));
   });
   _elm.Optics.Traversal.values = {_op: _op
                                  ,Traversal: Traversal
                                  ,view: view
                                  ,preview: preview
                                  ,set: set
                                  ,over: over
                                  ,single: single
                                  ,maybeToList: maybeToList
                                  ,pToT: pToT
                                  ,lToT: lToT};
   return _elm.Optics.Traversal.values;
};Elm.Optics = Elm.Optics || {};
Elm.Optics.Lens = Elm.Optics.Lens || {};
Elm.Optics.Lens.make = function (_elm) {
   "use strict";
   _elm.Optics = _elm.Optics || {};
   _elm.Optics.Lens = _elm.Optics.Lens || {};
   if (_elm.Optics.Lens.values)
   return _elm.Optics.Lens.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Optics.Lens",
   $Basics = Elm.Basics.make(_elm);
   var set = function (_) {
      return _.set;
   };
   _op["@~"] = set;
   var view = function (_) {
      return _.view;
   };
   _op["^@"] = $Basics.flip(view);
   var over = F3(function (ln,
   f,
   s) {
      return A3(set,
      ln,
      f(A2(view,ln,s)),
      s);
   });
   _op["@%"] = over;
   var Lens = F2(function (a,b) {
      return {_: {}
             ,set: b
             ,view: a};
   });
   _op["@@"] = F2(function (l,m) {
      return A2(Lens,
      function ($) {
         return view(m)(view(l)($));
      },
      function ($) {
         return over(l)(set(m)($));
      });
   });
   var fst_ = A2(Lens,
   $Basics.fst,
   F2(function (b,_v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: b
                   ,_1: _v0._1};}
         _E.Case($moduleName,
         "on line 33, column 32 to 36");
      }();
   }));
   var snd_ = A2(Lens,
   $Basics.snd,
   F2(function (b,_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: _v4._0
                   ,_1: b};}
         _E.Case($moduleName,
         "on line 36, column 32 to 36");
      }();
   }));
   _elm.Optics.Lens.values = {_op: _op
                             ,set: set
                             ,view: view
                             ,over: over
                             ,fst_: fst_
                             ,snd_: snd_
                             ,Lens: Lens};
   return _elm.Optics.Lens.values;
};Elm.Optics = Elm.Optics || {};
Elm.Optics.Prism = Elm.Optics.Prism || {};
Elm.Optics.Prism.make = function (_elm) {
   "use strict";
   _elm.Optics = _elm.Optics || {};
   _elm.Optics.Prism = _elm.Optics.Prism || {};
   if (_elm.Optics.Prism.values)
   return _elm.Optics.Prism.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Optics.Prism",
   $Basics = Elm.Basics.make(_elm),
   $Either = Elm.Either.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var set = F2(function (pr,b) {
      return function ($) {
         return A2($Either.either,
         $Basics.identity,
         $Basics.always(pr.inject(b)))(pr.matching($));
      };
   });
   _op["?~"] = set;
   var over = F3(function (pr,
   f,
   s) {
      return A2($Either.either,
      $Basics.identity,
      function (a) {
         return A3(set,pr,f(a),s);
      })(pr.matching(s));
   });
   _op["?%"] = over;
   var preview = function (pr) {
      return function ($) {
         return A2($Either.either,
         $Basics.always($Maybe.Nothing),
         $Maybe.Just)(pr.matching($));
      };
   };
   _op["^?"] = $Basics.flip(preview);
   var Prism = F2(function (a,b) {
      return {_: {}
             ,inject: a
             ,matching: b};
   });
   _op["??"] = F2(function (p,q) {
      return A2(Prism,
      function ($) {
         return p.inject(q.inject($));
      },
      function ($) {
         return A2($Either.either,
         $Either.Left,
         function ($) {
            return A2($Either.either,
            function ($) {
               return $Either.Left(p.inject($));
            },
            $Either.Right)(q.matching($));
         })(p.matching($));
      });
   });
   var just_ = A2(Prism,
   $Maybe.Just,
   function (s) {
      return function () {
         switch (s.ctor)
         {case "Just":
            return $Either.Right(s._0);
            case "Nothing":
            return $Either.Left($Maybe.Nothing);}
         _E.Case($moduleName,
         "between lines 36 and 38");
      }();
   });
   var nothing_ = A2(Prism,
   $Basics.always($Maybe.Nothing),
   function (s) {
      return function () {
         switch (s.ctor)
         {case "Just":
            return $Either.Left(s);
            case "Nothing":
            return $Either.Right({ctor: "_Tuple0"});}
         _E.Case($moduleName,
         "between lines 41 and 43");
      }();
   });
   _elm.Optics.Prism.values = {_op: _op
                              ,set: set
                              ,preview: preview
                              ,over: over
                              ,just_: just_
                              ,nothing_: nothing_
                              ,Prism: Prism};
   return _elm.Optics.Prism.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   if (_elm.Pseudorandom.values)
   return _elm.Pseudorandom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom",
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Pseudorandom$Infix = Elm.Pseudorandom.Infix.make(_elm),
   $Pseudorandom$Internal = Elm.Pseudorandom.Internal.make(_elm);
   var get = F2(function (n,r) {
      return $Basics.fst(r(n));
   });
   var $int = function (r) {
      return function () {
         var s$ = $Pseudorandom$Internal.xorshift(r);
         return {ctor: "_Tuple2"
                ,_0: s$
                ,_1: s$};
      }();
   };
   var $float = function ($) {
      return function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: $Basics.toFloat($Basics.abs(_v0._0) - 1) / (0 - $Pseudorandom$Internal.minInt)
                      ,_1: _v0._1};}
            _E.Case($moduleName,
            "on line 72, column 22 to 54");
         }();
      }($int($));
   };
   var range = function (rn) {
      return function ($) {
         return function (_v4) {
            return function () {
               switch (_v4.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: A2($Pseudorandom$Internal.roundClamp,
                         rn,
                         _v4._0)
                         ,_1: _v4._1};}
               _E.Case($moduleName,
               "on line 76, column 25 to 43");
            }();
         }($int($));
      };
   };
   var andThen = F2(function (x,
   y) {
      return A2($Pseudorandom$Infix._op["=<<"],
      x,
      y);
   });
   var apply = F2(function (x,y) {
      return A2($Pseudorandom$Infix._op["<*>"],
      x,
      y);
   });
   var constant = F2(function (v0,
   v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   });
   var combine = A2($List.foldr,
   F2(function (x,xs) {
      return A2($Pseudorandom$Infix._op["<*>"],
      A2($Pseudorandom$Infix._op["<$>"],
      F2(function (x,y) {
         return A2($List._op["::"],
         x,
         y);
      }),
      x),
      xs);
   }),
   constant(_L.fromArray([])));
   var map = function (f) {
      return function ($) {
         return combine($List.map(f)($));
      };
   };
   var combineA = A2($Array.foldl,
   F2(function (x,xs) {
      return A2($Pseudorandom$Infix._op["<*>"],
      A2($Pseudorandom$Infix._op["<$>"],
      $Array.push,
      x),
      xs);
   }),
   constant($Array.empty));
   var mapA = function (f) {
      return function ($) {
         return combineA($Array.map(f)($));
      };
   };
   var lift = F2(function (x,y) {
      return A2($Pseudorandom$Infix._op["<$>"],
      x,
      y);
   });
   _elm.Pseudorandom.values = {_op: _op
                              ,constant: constant
                              ,andThen: andThen
                              ,combine: combine
                              ,map: map
                              ,$int: $int
                              ,$float: $float
                              ,range: range
                              ,get: get
                              ,mapA: mapA
                              ,combineA: combineA
                              ,lift: lift
                              ,apply: apply};
   return _elm.Pseudorandom.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.Infix = Elm.Pseudorandom.Infix || {};
Elm.Pseudorandom.Infix.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   _elm.Pseudorandom.Infix = _elm.Pseudorandom.Infix || {};
   if (_elm.Pseudorandom.Infix.values)
   return _elm.Pseudorandom.Infix.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom.Infix",
   $Basics = Elm.Basics.make(_elm),
   $Pseudorandom$Internal = Elm.Pseudorandom.Internal.make(_elm);
   _op["=<<"] = F2(function (f,m) {
      return function ($) {
         return $Basics.uncurry(f)(m($));
      };
   });
   _op["<=<"] = F3(function (f,
   g,
   x) {
      return A2(_op["=<<"],f,g(x));
   });
   _op["<*>"] = F2(function (rf,
   ra) {
      return function ($) {
         return function (_v0) {
            return function () {
               switch (_v0.ctor)
               {case "_Tuple2":
                  return function (_v4) {
                       return function () {
                          switch (_v4.ctor)
                          {case "_Tuple2":
                             return {ctor: "_Tuple2"
                                    ,_0: _v4._0(_v0._0)
                                    ,_1: _v4._1};}
                          _E.Case($moduleName,
                          "on line 23, column 39 to 46");
                       }();
                    }(rf(_v0._1));}
               _E.Case($moduleName,
               "on line 23, column 39 to 56");
            }();
         }(ra($));
      };
   });
   _op["<$>"] = F2(function (f,r) {
      return function ($) {
         return function (_v8) {
            return function () {
               switch (_v8.ctor)
               {case "_Tuple2":
                  return {ctor: "_Tuple2"
                         ,_0: f(_v8._0)
                         ,_1: _v8._1};}
               _E.Case($moduleName,
               "on line 18, column 24 to 30");
            }();
         }(r($));
      };
   });
   _elm.Pseudorandom.Infix.values = {_op: _op};
   return _elm.Pseudorandom.Infix.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.Internal = Elm.Pseudorandom.Internal || {};
Elm.Pseudorandom.Internal.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   _elm.Pseudorandom.Internal = _elm.Pseudorandom.Internal || {};
   if (_elm.Pseudorandom.Internal.values)
   return _elm.Pseudorandom.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom.Internal",
   $Basics = Elm.Basics.make(_elm),
   $Bitwise = Elm.Bitwise.make(_elm);
   var roundClamp = F2(function (_v0,
   i) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _v0._0 + A2($Basics._op["%"],
              i - _v0._0,
              _v0._1 - _v0._0 + 1);}
         _E.Case($moduleName,
         "on line 23, column 23 to 47");
      }();
   });
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var bit32 = 4294967295;
   var c = 5;
   var b = 17;
   var a = 13;
   var xorshift = function (s) {
      return function () {
         var x = A2($Bitwise.xor,
         s,
         A2($Bitwise.shiftLeft,s,a));
         var y = A2($Bitwise.xor,
         x,
         A2($Bitwise.shiftRight,x,b));
         return A2($Bitwise.xor,
         y,
         A2($Bitwise.shiftLeft,y,c));
      }();
   };
   _elm.Pseudorandom.Internal.values = {_op: _op
                                       ,xorshift: xorshift
                                       ,roundClamp: roundClamp
                                       ,maxInt: maxInt
                                       ,minInt: minInt};
   return _elm.Pseudorandom.Internal.values;
};Elm.Slider = Elm.Slider || {};
Elm.Slider.make = function (_elm) {
   "use strict";
   _elm.Slider = _elm.Slider || {};
   if (_elm.Slider.values)
   return _elm.Slider.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Slider",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Native$Slider = Elm.Native.Slider.make(_elm);
   var slider = $Native$Slider.slider;
   var defaultSlider = {_: {}
                       ,disabled: false
                       ,horizontal: true
                       ,length: 100
                       ,max: 100
                       ,min: 0
                       ,step: 1
                       ,value: 0};
   var SliderStyle = F7(function (a,
   b,
   c,
   d,
   e,
   f,
   g) {
      return {_: {}
             ,disabled: b
             ,horizontal: a
             ,length: c
             ,max: e
             ,min: d
             ,step: f
             ,value: g};
   });
   _elm.Slider.values = {_op: _op
                        ,SliderStyle: SliderStyle
                        ,defaultSlider: defaultSlider
                        ,slider: slider};
   return _elm.Slider.values;
};Elm.Ticks = Elm.Ticks || {};
Elm.Ticks.make = function (_elm) {
   "use strict";
   _elm.Ticks = _elm.Ticks || {};
   if (_elm.Ticks.values)
   return _elm.Ticks.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Ticks",
   $Basics = Elm.Basics.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm);
   var stepTicks = $Signal.countIf(function (s) {
      return function () {
         switch (s.ctor)
         {case "Step": return true;}
         return false;
      }();
   });
   var reset = F2(function (s,t) {
      return function () {
         var f = F2(function (_v1,
         _v2) {
            return function () {
               switch (_v2.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v1.ctor)
                       {case "_Tuple2":
                          return function () {
                               switch (_v1._0.ctor)
                               {case "Reseed":
                                  return {ctor: "_Tuple2"
                                         ,_0: 0
                                         ,_1: _v1._1};
                                  case "Reset":
                                  return {ctor: "_Tuple2"
                                         ,_0: 0
                                         ,_1: _v1._1};}
                               return {ctor: "_Tuple2"
                                      ,_0: _v1._1 - _v2._1
                                      ,_1: _v2._1};
                            }();}
                       _E.Case($moduleName,
                       "between lines 30 and 33");
                    }();}
               _E.Case($moduleName,
               "between lines 30 and 33");
            }();
         });
         return A2($Signal._op["<~"],
         $Basics.fst,
         A3($Signal.foldp,
         f,
         {ctor: "_Tuple2",_0: 0,_1: 0},
         A2($Signal._op["~"],
         A2($Signal._op["<~"],
         F2(function (v0,v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }),
         s),
         t)));
      }();
   });
   var playing = function (s) {
      return function () {
         switch (s.ctor)
         {case "Play": return true;}
         return false;
      }();
   };
   var playTicks = F2(function (r,
   s) {
      return function () {
         var sig = A2($Signal._op["~"],
         A2($Signal._op["<~"],
         F2(function (v0,v1) {
            return {ctor: "_Tuple2"
                   ,_0: v0
                   ,_1: v1};
         }),
         A2($Time.fpsWhen,
         30,
         A2($Signal._op["<~"],
         playing,
         s))),
         r);
         return A2($Signal._op["<~"],
         $Basics.floor,
         A3($Signal.foldp,
         F2(function (_v11,acc) {
            return function () {
               switch (_v11.ctor)
               {case "_Tuple2":
                  return acc + _v11._0 * _v11._1;}
               _E.Case($moduleName,
               "on line 44, column 48 to 60");
            }();
         }),
         0,
         sig));
      }();
   });
   var totalTicks = F2(function (r,
   s) {
      return $Signal.dropRepeats(A2($Signal._op["~"],
      A2($Signal._op["<~"],
      F2(function (x,y) {
         return x + y;
      }),
      A2(reset,s,stepTicks(s))),
      A2(reset,s,A2(playTicks,r,s))));
   });
   var Reseed = {ctor: "Reseed"};
   var Reset = {ctor: "Reset"};
   var Pause = {ctor: "Pause"};
   var run = $Graphics$Input.input(Pause);
   var Play = {ctor: "Play"};
   var Step = {ctor: "Step"};
   var playControls = A2($Graphics$Element.flow,
   $Graphics$Element.down,
   _L.fromArray([A2($Graphics$Element.flow,
                $Graphics$Element.right,
                _L.fromArray([A3($Graphics$Input.button,
                             run.handle,
                             Play,
                             "Play")
                             ,A3($Graphics$Input.button,
                             run.handle,
                             Pause,
                             "Pause")
                             ,A3($Graphics$Input.button,
                             run.handle,
                             Step,
                             "Step")]))
                ,A2($Graphics$Element.spacer,
                1,
                5)
                ,A2($Graphics$Element.flow,
                $Graphics$Element.right,
                _L.fromArray([A3($Graphics$Input.button,
                             run.handle,
                             Reset,
                             "Reset")
                             ,A3($Graphics$Input.button,
                             run.handle,
                             Reseed,
                             "Reseed")]))]));
   _elm.Ticks.values = {_op: _op
                       ,playControls: playControls
                       ,Step: Step
                       ,Play: Play
                       ,Pause: Pause
                       ,Reset: Reset
                       ,Reseed: Reseed
                       ,run: run
                       ,playing: playing
                       ,reset: reset
                       ,stepTicks: stepTicks
                       ,playTicks: playTicks
                       ,totalTicks: totalTicks};
   return _elm.Ticks.values;
};