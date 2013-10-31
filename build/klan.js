Elm.Main = Elm.Main || {};
Elm.Main.make = function (elm)
                {
                  elm.Main = elm.Main || {};
                  if (elm.Main.values)
                  return elm.Main.values;
                  var N = Elm.Native,
                      _N = N.Utils.make(elm),
                      _L = N.List.make(elm),
                      _E = N.Error.make(elm),
                      _J = N.JavaScript.make(elm),
                      $moduleName = "Main";
                  var Text = Elm.Text.make(elm);
                  var Text = Elm.Text.make(elm);
                  var Basics = Elm.Basics.make(elm);
                  var Signal = Elm.Signal.make(elm);
                  var List = Elm.List.make(elm);
                  var Maybe = Elm.Maybe.make(elm);
                  var Time = Elm.Time.make(elm);
                  var Prelude = Elm.Prelude.make(elm);
                  var Graphics = Graphics || {};
                  Graphics.Element = Elm.Graphics.Element.make(elm);
                  var Color = Elm.Color.make(elm);
                  var Graphics = Graphics || {};
                  Graphics.Collage = Elm.Graphics.Collage.make(elm);
                  var Color = Elm.Color.make(elm);
                  var Bracket = Elm.Bracket.make(elm);
                  var Either = Elm.Either.make(elm);
                  var Window = Elm.Window.make(elm);
                  var _op = {};
                  var players = A2(List.map,
                                   function (i)
                                   {
                                     return _L.append("Player ",Prelude.show(i));
                                   },
                                   _L.range(1,8));
                  var Two = F2(function (a,b)
                               {
                                 return {ctor: "Two", _0: a, _1: b};
                               });
                  var Team = function (a)
                             {
                               return {ctor: "Team", _0: a};
                             };
                  var Player = function (a)
                               {
                                 return {ctor: "Player", _0: a};
                               };
                  var brackName = function (b)
                                  {
                                    return function ()
                                           {
                                             switch (b.ctor)
                                             {case
                                              "Player" :
                                                return function (_)
                                                       {
                                                         return _.name;
                                                       }(b._0);
                                              case
                                              "Team" :
                                                return function (_)
                                                       {
                                                         return _.name;
                                                       }(b._0);}
                                             _E.Case($moduleName,"between lines 15 and 17");
                                           }();
                                  };
                  var brackEq = F2(function (b1,b2)
                                   {
                                     return _N.eq(brackName(b1),brackName(b2));
                                   });
                  var brackScore = function (b)
                                   {
                                     return function ()
                                            {
                                              switch (b.ctor)
                                              {case
                                               "Player" :
                                                 return function (_)
                                                        {
                                                          return _.score;
                                                        }(b._0);
                                               case
                                               "Team" :
                                                 return function (_)
                                                        {
                                                          return _.score;
                                                        }(b._0);}
                                              _E.Case($moduleName,"between lines 20 and 22");
                                            }();
                                   };
                  var renderBrack = function (b)
                                    {
                                      return function ()
                                             {
                                               var contain = A2(Graphics.Element.container,200,20);
                                               return Graphics.Element.layers(_J.toList([contain(Graphics.Element.midLeft)(Text.plainText(brackName(b))),
                                                                                         contain(Graphics.Element.midRight)(Text.plainText(Prelude.show(brackScore(b))))]));
                                             }();
                                    };
                  var One = function (a)
                            {
                              return {ctor: "One", _0: a};
                            };
                  var Empty = {ctor: "Empty"};
                  var matchEq = F2(function (m1,m2)
                                   {
                                     return function ()
                                            {
                                              var _case6 = {ctor: "_Tuple2", _0: m1, _1: m2};
                                              switch (_case6.ctor)
                                              {case
                                               "_Tuple2" :
                                                 switch (_case6._0.ctor)
                                                 {case
                                                  "Empty" :
                                                    switch (_case6._1.ctor)
                                                    {case
                                                     "Empty" :
                                                       return true;}
                                                    break;
                                                  case
                                                  "One" :
                                                    switch (_case6._1.ctor)
                                                    {case
                                                     "One" :
                                                       return A2(brackEq,
                                                                 _case6._0._0,
                                                                 _case6._1._0);}
                                                    break;
                                                  case
                                                  "Two" :
                                                    switch (_case6._1.ctor)
                                                    {case
                                                     "Two" :
                                                       return A2(brackEq,
                                                                 _case6._0._0,
                                                                 _case6._1._0) && A2(brackEq,
                                                                                     _case6._0._1,
                                                                                     _case6._1._1);}
                                                    break;}
                                                 break;}
                                              return false;
                                            }();
                                   });
                  var updateScore = F2(function (m,b)
                                       {
                                         return A2(Bracket.mapBracket,
                                                   function (m$)
                                                   {
                                                     return A2(matchEq,m,m$) ? m : m$;
                                                   },
                                                   b);
                                       });
                  var maxScore = function (m)
                                 {
                                   return function ()
                                          {
                                            switch (m.ctor)
                                            {case
                                             "Empty" :
                                               return Maybe.Nothing;
                                             case
                                             "One" :
                                               return Maybe.Just(m._0);
                                             case
                                             "Two" :
                                               return _N.cmp(brackScore(m._0),
                                                             brackScore(m._1)) > 0 ? Maybe.Just(m._0) : _N.cmp(brackScore(m._1),
                                                                                                               brackScore(m._0)) > 0 ? Maybe.Just(m._1) : Maybe.Nothing;}
                                            _E.Case($moduleName,"between lines 41 and 46");
                                          }();
                                 };
                  var renderMatch = function (m)
                                    {
                                      return function ()
                                             {
                                               switch (m.ctor)
                                               {case
                                                "Empty" :
                                                  return {ctor: "_Tuple3", _0: 200, _1: 0, _2: Graphics.Collage.toForm(A2(Graphics.Element.spacer,
                                                                                                                          0,
                                                                                                                          0))};
                                                case
                                                "One" :
                                                  return {ctor: "_Tuple3", _0: 200, _1: 20, _2: Graphics.Collage.toForm(renderBrack(m._0))};
                                                case
                                                "Two" :
                                                  return function ()
                                                         {
                                                           var cont = A3(Graphics.Element.container,
                                                                         200,
                                                                         20,
                                                                         Graphics.Element.midLeft);
                                                           return {ctor: "_Tuple3", _0: 200, _1: 40, _2: Graphics.Collage.toForm(Graphics.Element.flow(Graphics.Element.down)({ctor: "::", _0: cont(renderBrack(m._0)), _1: _J.toList([cont(renderBrack(m._1))])}))};
                                                         }();}
                                               _E.Case($moduleName,"between lines 58 and 67");
                                             }();
                                    };
                  var winners = F2(function (b1,b2)
                                   {
                                     return function ()
                                            {
                                              var winner = function (b)
                                                           {
                                                             return function ()
                                                                    {
                                                                      switch (b.ctor)
                                                                      {case
                                                                       "InnerNode" :
                                                                         return maxScore(b._0);
                                                                       case
                                                                       "Leaf" :
                                                                         return maxScore(b._0);}
                                                                      _E.Case($moduleName,
                                                                              "between lines 118 and 121");
                                                                    }();
                                                           };
                                              return function ()
                                                     {
                                                       var _case28 = {ctor: "_Tuple2", _0: winner(b1), _1: winner(b2)};
                                                       switch (_case28.ctor)
                                                       {case
                                                        "_Tuple2" :
                                                          switch (_case28._0.ctor)
                                                          {case
                                                           "Just" :
                                                             switch (_case28._1.ctor)
                                                             {case
                                                              "Just" :
                                                                return A2(Two,
                                                                          _case28._0._0,
                                                                          _case28._1._0);}
                                                             break;}
                                                          break;}
                                                       return Empty;
                                                     }();
                                            }();
                                   });
                  var updateBracket = function (b)
                                      {
                                        return function ()
                                               {
                                                 switch (b.ctor)
                                                 {case
                                                  "InnerNode" :
                                                    return function ()
                                                           {
                                                             var b2$ = updateBracket(b._2);
                                                             var b1$ = updateBracket(b._1);
                                                             return function ()
                                                                    {
                                                                      switch (b._0.ctor)
                                                                      {case
                                                                       "Empty" :
                                                                         return A3(Bracket.InnerNode,
                                                                                   A2(winners,
                                                                                      b1$,
                                                                                      b2$),
                                                                                   b1$,
                                                                                   b2$);}
                                                                      return b;
                                                                    }();
                                                           }();
                                                  case
                                                  "Leaf" :
                                                    return b;}
                                                 _E.Case($moduleName,"between lines 129 and 137");
                                               }();
                                      };
                  var Brack_desc = F2(function (a,b)
                                      {
                                        return {_: {}, name: b, score: a};
                                      });
                  var player = function (string)
                               {
                                 return Player(A2(Brack_desc,0,string));
                               };
                  var fromList = function (players)
                                 {
                                   return function ()
                                          {
                                            var minDepth = function (b)
                                                           {
                                                             return function ()
                                                                    {
                                                                      switch (b.ctor)
                                                                      {case
                                                                       "InnerNode" :
                                                                         return 1 + A2(Basics.min,
                                                                                       minDepth(b._1),
                                                                                       minDepth(b._2));
                                                                       case
                                                                       "Leaf" :
                                                                         return 0;}
                                                                      _E.Case($moduleName,
                                                                              "between lines 73 and 76");
                                                                    }();
                                                           };
                                            var hasFreeSpot = function (b)
                                                              {
                                                                return function ()
                                                                       {
                                                                         switch (b.ctor)
                                                                         {case
                                                                          "InnerNode" :
                                                                            return hasFreeSpot(b._1) || hasFreeSpot(b._2);
                                                                          case
                                                                          "Leaf" :
                                                                            return function ()
                                                                                   {
                                                                                     switch (b._0.ctor)
                                                                                     {case
                                                                                      "Empty" :
                                                                                        return true;
                                                                                      case
                                                                                      "One" :
                                                                                        return true;}
                                                                                     return false;
                                                                                   }();}
                                                                         _E.Case($moduleName,
                                                                                 "between lines 77 and 83");
                                                                       }();
                                                              };
                                            var chooseSide = F2(function (p,b)
                                                                {
                                                                  return function ()
                                                                         {
                                                                           switch (b.ctor)
                                                                           {case
                                                                            "InnerNode" :
                                                                              return function ()
                                                                                     {
                                                                                       var b2dep = minDepth(b._2);
                                                                                       var b1dep = minDepth(b._1);
                                                                                       return _N.eq(b1dep,
                                                                                                    b2dep) ? A3(Bracket.InnerNode,
                                                                                                                Empty,
                                                                                                                Bracket.Leaf(One(player(p))),
                                                                                                                b) : _N.cmp(b1dep,
                                                                                                                            b2dep) > 0 ? A3(Bracket.InnerNode,
                                                                                                                                            Empty,
                                                                                                                                            b._1,
                                                                                                                                            A2(consBracket,
                                                                                                                                               p,
                                                                                                                                               b._2)) : A3(Bracket.InnerNode,
                                                                                                                                                           Empty,
                                                                                                                                                           A2(consBracket,
                                                                                                                                                              p,
                                                                                                                                                              b._1),
                                                                                                                                                           b._2);
                                                                                     }();
                                                                            case
                                                                            "Leaf" :
                                                                              return Bracket.Leaf(b._0);}
                                                                           _E.Case($moduleName,
                                                                                   "between lines 85 and 97");
                                                                         }();
                                                                });
                                            var consBracket = F2(function (p,b)
                                                                 {
                                                                   return function ()
                                                                          {
                                                                            switch (b.ctor)
                                                                            {case
                                                                             "InnerNode" :
                                                                               return hasFreeSpot(b._1) ? A3(Bracket.InnerNode,
                                                                                                             b._0,
                                                                                                             A2(consBracket,
                                                                                                                p,
                                                                                                                b._1),
                                                                                                             b._2) : hasFreeSpot(b._2) ? A3(Bracket.InnerNode,
                                                                                                                                            b._0,
                                                                                                                                            b._1,
                                                                                                                                            A2(consBracket,
                                                                                                                                               p,
                                                                                                                                               b._2)) : A2(chooseSide,
                                                                                                                                                           p,
                                                                                                                                                           b);
                                                                             case
                                                                             "Leaf" :
                                                                               return function ()
                                                                                      {
                                                                                        switch (b._0.ctor)
                                                                                        {case
                                                                                         "Empty" :
                                                                                           return Bracket.Leaf(One(player(p)));
                                                                                         case
                                                                                         "One" :
                                                                                           return Bracket.Leaf(A2(Two,
                                                                                                                  b._0._0,
                                                                                                                  player(p)));
                                                                                         case
                                                                                         "Two" :
                                                                                           return A3(Bracket.InnerNode,
                                                                                                     Empty,
                                                                                                     Bracket.Leaf(One(player(p))),
                                                                                                     b);}
                                                                                        _E.Case($moduleName,
                                                                                                "between lines 100 and 104");
                                                                                      }();}
                                                                            _E.Case($moduleName,
                                                                                    "between lines 99 and 108");
                                                                          }();
                                                                 });
                                            var build = F2(function (ps,b)
                                                           {
                                                             return function ()
                                                                    {
                                                                      switch (ps.ctor)
                                                                      {case
                                                                       "::" :
                                                                         switch (ps._1.ctor)
                                                                         {case
                                                                          "[]" :
                                                                            return A2(consBracket,
                                                                                      ps._0,
                                                                                      b);}
                                                                         return A2(build,
                                                                                   ps._1,
                                                                                   A2(consBracket,
                                                                                      ps._0,
                                                                                      b));}
                                                                      _E.Case($moduleName,
                                                                              "between lines 110 and 113");
                                                                    }();
                                                           });
                                            return A2(build,players,Bracket.Leaf(Empty));
                                          }();
                                 };
                  var playerWithScore = F2(function (name,score)
                                           {
                                             return Player(A2(Brack_desc,score,name));
                                           });
                  var bracket = updateBracket(updateScore(A2(Two,
                                                             A2(playerWithScore,"Player 5",2),
                                                             player("Player 6")))(updateScore(A2(Two,
                                                                                                 player("Player 7"),
                                                                                                 A2(playerWithScore,
                                                                                                    "Player 8",
                                                                                                    2)))(fromList(players))));
                  var render = function (input)
                               {
                                 return function ()
                                        {
                                          var $ = A2(Bracket.renderBracket,bracket,renderMatch),
                                              bw = $._0,
                                              bh = $._1,
                                              brkt = $._2;
                                          var br = A2(List.map,
                                                      Graphics.Collage.moveX(Basics.toFloat(bw) / 2),
                                                      brkt);
                                          var $ = {ctor: "_Tuple2", _0: Basics.fst(input), _1: Basics.snd(input)},
                                              w = $._0,
                                              h = $._1;
                                          return A2(Graphics.Collage.collage,
                                                    w,
                                                    h)(_L.append(_J.toList([Graphics.Collage.filled(Color.white)(A2(Graphics.Collage.rect,
                                                                                                                    Basics.toFloat(w),
                                                                                                                    Basics.toFloat(h))),
                                                                            Graphics.Collage.toForm(A4(Graphics.Element.container,
                                                                                                       w,
                                                                                                       h,
                                                                                                       Graphics.Element.midTop,
                                                                                                       A3(Graphics.Element.image,
                                                                                                          400,
                                                                                                          100,
                                                                                                          "Banner4.png")))]),
                                                                 _J.toList([Graphics.Collage.group(br)])));
                                        }();
                               };
                  var main = A2(Signal.lift,render,Window.dimensions);
                  elm.Main.values = {_op: _op, player: player, playerWithScore: playerWithScore, brackName: brackName, brackScore: brackScore, brackEq: brackEq, matchEq: matchEq, maxScore: maxScore, renderBrack: renderBrack, renderMatch: renderMatch, fromList: fromList, winners: winners, updateBracket: updateBracket, updateScore: updateScore, players: players, bracket: bracket, render: render, main: main, Player: Player, Team: Team, Empty: Empty, One: One, Two: Two, Brack_desc: Brack_desc};
                  return elm.Main.values;
                };Elm.Bracket = Elm.Bracket || {};
Elm.Bracket.make = function (elm)
                   {
                     elm.Bracket = elm.Bracket || {};
                     if (elm.Bracket.values)
                     return elm.Bracket.values;
                     var N = Elm.Native,
                         _N = N.Utils.make(elm),
                         _L = N.List.make(elm),
                         _E = N.Error.make(elm),
                         _J = N.JavaScript.make(elm),
                         $moduleName = "Bracket";
                     var Text = Elm.Text.make(elm);
                     var Text = Elm.Text.make(elm);
                     var Basics = Elm.Basics.make(elm);
                     var Signal = Elm.Signal.make(elm);
                     var List = Elm.List.make(elm);
                     var Maybe = Elm.Maybe.make(elm);
                     var Time = Elm.Time.make(elm);
                     var Prelude = Elm.Prelude.make(elm);
                     var Graphics = Graphics || {};
                     Graphics.Element = Elm.Graphics.Element.make(elm);
                     var Color = Elm.Color.make(elm);
                     var Graphics = Graphics || {};
                     Graphics.Collage = Elm.Graphics.Collage.make(elm);
                     var _op = {};
                     var Leaf = function (a)
                                {
                                  return {ctor: "Leaf", _0: a};
                                };
                     var InnerNode = F3(function (a,b,c)
                                        {
                                          return {ctor: "InnerNode", _0: a, _1: b, _2: c};
                                        });
                     var mapBracket = F2(function (fun,b)
                                         {
                                           return function ()
                                                  {
                                                    switch (b.ctor)
                                                    {case
                                                     "InnerNode" :
                                                       return A3(InnerNode,
                                                                 fun(b._0),
                                                                 A2(mapBracket,fun,b._1),
                                                                 A2(mapBracket,fun,b._2));
                                                     case
                                                     "Leaf" :
                                                       return Leaf(fun(b._0));}
                                                    _E.Case($moduleName,"between lines 9 and 11");
                                                  }();
                                         });
                     var renderBracket = F2(function (b,draw)
                                            {
                                              return function ()
                                                     {
                                                       switch (b.ctor)
                                                       {case
                                                        "InnerNode" :
                                                          return function ()
                                                                 {
                                                                   var $ = A2(renderBracket,
                                                                              b._2,
                                                                              draw),
                                                                       w2 = $._0,
                                                                       h2 = $._1,
                                                                       right = $._2;
                                                                   var $ = A2(renderBracket,
                                                                              b._1,
                                                                              draw),
                                                                       w1 = $._0,
                                                                       h1 = $._1,
                                                                       left = $._2;
                                                                   var $ = draw(b._0),
                                                                       w = $._0,
                                                                       h = $._1,
                                                                       drawn = $._2;
                                                                   var barrw = A2(Graphics.Collage.traced,
                                                                                  Graphics.Collage.dashed(Color.black),
                                                                                  Graphics.Collage.path(_J.toList([{ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 50, _1: Basics.toFloat(h2)},
                                                                                                                   {ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 25, _1: Basics.toFloat(h2)},
                                                                                                                   {ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 25, _1: 0},
                                                                                                                   {ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2, _1: 0}])));
                                                                   var left$ = A2(List.map,
                                                                                  Graphics.Collage.move({ctor: "_Tuple2", _0: Basics.toFloat(0 - w - 50), _1: Basics.toFloat(0 - h1)}),
                                                                                  left);
                                                                   var right$ = A2(List.map,
                                                                                   Graphics.Collage.move({ctor: "_Tuple2", _0: Basics.toFloat(0 - w - 50), _1: Basics.toFloat(h2)}),
                                                                                   right);
                                                                   var tarrw = A2(Graphics.Collage.traced,
                                                                                  Graphics.Collage.dashed(Color.black),
                                                                                  Graphics.Collage.path(_J.toList([{ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 50, _1: Basics.toFloat(0 - h1)},
                                                                                                                   {ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 25, _1: Basics.toFloat(0 - h1)},
                                                                                                                   {ctor: "_Tuple2", _0: Basics.toFloat(0 - w) / 2 - 25, _1: 0}])));
                                                                   return {ctor: "_Tuple3", _0: w + w1, _1: h1 + h2, _2: {ctor: "::", _0: drawn, _1: {ctor: "::", _0: tarrw, _1: {ctor: "::", _0: barrw, _1: _L.append(left$,
                                                                                                                                                                                                                       right$)}}}};
                                                                 }();
                                                        case
                                                        "Leaf" :
                                                          return function ()
                                                                 {
                                                                   var $ = draw(b._0),
                                                                       w = $._0,
                                                                       h = $._1,
                                                                       drawn = $._2;
                                                                   return {ctor: "_Tuple3", _0: w, _1: h, _2: _J.toList([drawn])};
                                                                 }();}
                                                       _E.Case($moduleName,
                                                               "between lines 15 and 37");
                                                     }();
                                            });
                     elm.Bracket.values = {_op: _op, mapBracket: mapBracket, renderBracket: renderBracket, Leaf: Leaf, InnerNode: InnerNode};
                     return elm.Bracket.values;
                   };