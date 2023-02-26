(() => {
  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i = 0, l2 = arr.length; i < l2; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l2 = fs.length;
      var k2 = xs.length;
      var result = new Array(l2 * k2);
      var n = 0;
      for (var i = 0; i < l2; i++) {
        var f = fs[i];
        for (var j = 0; j < k2; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g2) {
        return function(x2) {
          return f(g2(x2));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x2) {
      return x2;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a3) {
        return f(a3)(b2);
      };
    };
  };
  var $$const = function(a3) {
    return function(v) {
      return a3;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l2 = arr.length;
      var result = new Array(l2);
      for (var i = 0; i < l2; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Unit/index.js
  var showUnit = {
    show: function(v) {
      return "unit";
    }
  };

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map112(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(f) {
      return function(x2) {
        return map112($$const(x2))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(ff2) {
      return function(x2) {
        return map112(function(f) {
          return f(x2);
        })(ff2);
      };
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applyFirst = function(dictApply) {
    var apply1 = apply(dictApply);
    var map26 = map(dictApply.Functor0());
    return function(a3) {
      return function(b2) {
        return apply1(map26($$const)(a3))(b2);
      };
    };
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map26 = map(dictApply.Functor0());
    return function(a3) {
      return function(b2) {
        return apply1(map26($$const(identity2))(a3))(b2);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map26 = map(dictApply.Functor0());
    return function(f) {
      return function(a3) {
        return function(b2) {
          return apply1(map26(f)(a3))(b2);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var when = function(dictApplicative) {
    var pure111 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure111(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure111 = pure(dictApplicative);
    return function(f) {
      return function(a3) {
        return apply3(pure111(f))(a3);
      };
    };
  };
  var applicativeArray = {
    pure: function(x2) {
      return [x2];
    },
    Apply0: function() {
      return applyArray;
    }
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind13 = bind(dictMonad.Bind1());
    var pure20 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a3) {
        return bind13(f)(function(f$prime) {
          return bind13(a3)(function(a$prime) {
            return pure20(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq7) {
      return function(gt) {
        return function(x2) {
          return function(y) {
            return x2 < y ? lt : x2 === y ? eq7 : gt;
          };
        };
      };
    };
  };
  var ordBooleanImpl = unsafeCompareImpl;
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x2) {
    return function(y) {
      return x2 - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x2) {
    return function(y) {
      return x2 + y | 0;
    };
  };
  var intMul = function(x2) {
    return function(y) {
      return x2 * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a3) {
      return sub1(zero2)(a3);
    };
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var ordBoolean = /* @__PURE__ */ function() {
    return {
      compare: ordBooleanImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqBoolean;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a22) {
        var v = compare32(a1)(a22);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var lessThan = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a22) {
        var v = compare32(a1)(a22);
        if (v instanceof LT) {
          return true;
        }
        ;
        return false;
      };
    };
  };
  var lessThanOrEq = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a22) {
        var v = compare32(a1)(a22);
        if (v instanceof GT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var max = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(x2) {
      return function(y) {
        var v = compare32(x2)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x2;
        }
        ;
        if (v instanceof GT) {
          return x2;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(x2) {
      return function(y) {
        var v = compare32(x2)(y);
        if (v instanceof LT) {
          return x2;
        }
        ;
        if (v instanceof EQ) {
          return x2;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showCharImpl = function(c2) {
    var code = c2.charCodeAt(0);
    if (code < 32 || code === 127) {
      switch (c2) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c2 === "'" || c2 === "\\" ? "'\\" + c2 + "'" : "'" + c2 + "'";
  };
  var showStringImpl = function(s) {
    var l2 = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function(c2, i) {
        switch (c2) {
          case '"':
          case "\\":
            return "\\" + c2;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k2 = i + 1;
        var empty5 = k2 < l2 && s[k2] >= "0" && s[k2] <= "9" ? "\\&" : "";
        return "\\" + c2.charCodeAt(0).toString(10) + empty5;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l2 = xs.length; i < l2; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };

  // output/Data.Generic.Rep/index.js
  var Inl = /* @__PURE__ */ function() {
    function Inl2(value0) {
      this.value0 = value0;
    }
    ;
    Inl2.create = function(value0) {
      return new Inl2(value0);
    };
    return Inl2;
  }();
  var Inr = /* @__PURE__ */ function() {
    function Inr2(value0) {
      this.value0 = value0;
    }
    ;
    Inr2.create = function(value0) {
      return new Inr2(value0);
    };
    return Inr2;
  }();
  var Product = /* @__PURE__ */ function() {
    function Product2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Product2.create = function(value0) {
      return function(value1) {
        return new Product2(value0, value1);
      };
    };
    return Product2;
  }();
  var NoArguments = /* @__PURE__ */ function() {
    function NoArguments2() {
    }
    ;
    NoArguments2.value = new NoArguments2();
    return NoArguments2;
  }();
  var Constructor = function(x2) {
    return x2;
  };
  var to = function(dict) {
    return dict.to;
  };
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a3) {
    return maybe(a3)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();
  var altMaybe = {
    alt: function(v) {
      return function(v1) {
        if (v instanceof Nothing) {
          return v1;
        }
        ;
        return v;
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var plusMaybe = /* @__PURE__ */ function() {
    return {
      empty: Nothing.value,
      Alt0: function() {
        return altMaybe;
      }
    };
  }();
  var alternativeMaybe = {
    Applicative0: function() {
      return applicativeMaybe;
    },
    Plus1: function() {
      return plusMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();

  // output/Effect/foreign.js
  var pureE = function(a3) {
    return function() {
      return a3;
    };
  };
  var bindE = function(a3) {
    return function(f) {
      return function() {
        return f(a3())();
      };
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x2) {
    return Math.min(Math.abs(x2), 2147483647);
  };
  var intDiv = function(x2) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x2 / y) : -Math.floor(x2 / -y);
    };
  };
  var intMod = function(x2) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x2 % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map26 = map(Monad0.Bind1().Apply0().Functor0());
    var pure20 = pure(Monad0.Applicative0());
    return function(a3) {
      return catchError1(map26(Right.create)(a3))(function($52) {
        return pure20(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x2) {
    return x2;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m2) {
        return f(m2);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var write = function(val) {
    return function(ref) {
      return function() {
        ref.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 93, column 3 - line 93, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 29, column 1 - line 31, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Control.Lazy/index.js
  var $runtime_lazy2 = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var lazyFn = {
    defer: function(f) {
      return function(x2) {
        return f(unit)(x2);
      };
    }
  };
  var defer = function(dict) {
    return dict.defer;
  };
  var fix = function(dictLazy) {
    var defer1 = defer(dictLazy);
    return function(f) {
      var $lazy_go = $runtime_lazy2("go", "Control.Lazy", function() {
        return defer1(function(v) {
          return f($lazy_go(25));
        });
      });
      var go = $lazy_go(25);
      return go;
    };
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var not = function(dict) {
    return dict.not;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a3) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a3))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq7 = eq(dictEq);
    return function(dictEq1) {
      var eq13 = eq(dictEq1);
      return {
        eq: function(x2) {
          return function(y) {
            return eq7(x2.value0)(y.value0) && eq13(x2.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare4 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x2) {
          return function(y) {
            var v = compare4(x2.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x2.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };
  var curry = function(f) {
    return function(a3) {
      return function(b2) {
        return f(new Tuple(a3, b2));
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.State.Trans/index.js
  var mapStateT = function(f) {
    return function(v) {
      return function($194) {
        return f(v($194));
      };
    };
  };
  var functorStateT = function(dictFunctor) {
    var map26 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map26(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind13 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind13(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure20 = pure(dictMonad.Applicative0());
    return {
      pure: function(a3) {
        return function(s) {
          return pure20(new Tuple(a3, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure20 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure20(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Data.Array/foreign.js
  var range = function(start2) {
    return function(end) {
      var step2 = start2 > end ? -1 : 1;
      var result = new Array(step2 * (end - start2) + 1);
      var i = start2, n = 0;
      while (i !== end) {
        result[n++] = i;
        i += step2;
      }
      result[n] = i;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count; i++) {
        result[n++] = value;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr5) {
      return function(xs) {
        return listToArray(foldr5(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty5) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty5({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i = 0, l2 = xs.length; i < l2; i++) {
            if (f(xs[i]))
              return just(i);
          }
          return nothing;
        };
      };
    };
  };
  var concat = function(xss) {
    if (xss.length <= 1e4) {
      return Array.prototype.concat.apply([], xss);
    }
    var result = [];
    for (var i = 0, l2 = xss.length; i < l2; i++) {
      var xs = xss[i];
      for (var j = 0, m2 = xs.length; j < m2; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
  var filter = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to3) {
      var mid;
      var i;
      var j;
      var k2;
      var x2;
      var y;
      var c2;
      mid = from3 + (to3 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
      if (to3 - mid > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to3);
      i = from3;
      j = mid;
      k2 = from3;
      while (i < mid && j < to3) {
        x2 = xs2[i];
        y = xs2[j];
        c2 = fromOrdering(compare4(x2)(y));
        if (c2 > 0) {
          xs1[k2++] = y;
          ++j;
        } else {
          xs1[k2++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k2++] = xs2[i++];
      }
      while (j < to3) {
        xs1[k2++] = xs2[j++];
      }
    }
    return function(compare4) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare4, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l2) {
        return l2.slice(s, e);
      };
    };
  };
  var zipWith = function(f) {
    return function(xs) {
      return function(ys) {
        var l2 = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l2);
        for (var i = 0; i < l2; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a3) {
      return function() {
        return f(a3());
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i = 0, l2 = as.length; i < l2; i++) {
          f(as[i])();
        }
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var functorST = {
    map: map_
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to3) {
      var mid;
      var i;
      var j;
      var k2;
      var x2;
      var y;
      var c2;
      mid = from3 + (to3 - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
      if (to3 - mid > 1)
        mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to3);
      i = from3;
      j = mid;
      k2 = from3;
      while (i < mid && j < to3) {
        x2 = xs2[i];
        y = xs2[j];
        c2 = fromOrdering(compare4(x2)(y));
        if (c2 > 0) {
          xs1[k2++] = y;
          ++j;
        } else {
          xs1[k2++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k2++] = xs2[i++];
      }
      while (j < to3) {
        xs1[k2++] = xs2[j++];
      }
    }
    return function(compare4) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare4, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Maybe.First/index.js
  var semigroupFirst = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v;
        }
        ;
        return v1;
      };
    }
  };
  var monoidFirst = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  }();

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x2) {
    return x2;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x2) {
    return x2;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var $$null = function(dictFoldable) {
    return foldr(dictFoldable)(function(v) {
      return function(v1) {
        return false;
      };
    })(true);
  };
  var traverse_ = function(dictApplicative) {
    var applySecond6 = applySecond(dictApplicative.Apply0());
    var pure20 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($449) {
          return applySecond6(f($449));
        })(pure20(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go = function(v) {
            return function(x2) {
              if (v.init) {
                return {
                  init: false,
                  acc: x2
                };
              }
              ;
              return {
                init: false,
                acc: append8(v.acc)(append8(sep)(x2))
              };
            };
          };
          return foldl22(go)({
            init: true,
            acc: mempty3
          })(xs).acc;
        };
      };
    };
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append8 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x2) {
          return function(acc) {
            return append8(f(x2))(acc);
          };
        })(mempty3);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var lookup = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable)(monoidFirst);
    return function(dictEq) {
      var eq23 = eq(dictEq);
      return function(a3) {
        var $455 = foldMap2(function(v) {
          var $439 = eq23(a3)(v.value0);
          if ($439) {
            return new Just(v.value1);
          }
          ;
          return Nothing.value;
        });
        return function($456) {
          return unwrap2($455($456));
        };
      };
    };
  };
  var fold = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictMonoid) {
      return foldMap2(dictMonoid)(identity4);
    };
  };
  var any = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
    };
  };
  var elem = function(dictFoldable) {
    var any1 = any(dictFoldable)(heytingAlgebraBoolean);
    return function(dictEq) {
      var $457 = eq(dictEq);
      return function($458) {
        return any1($457($458));
      };
    };
  };
  var or = function(dictFoldable) {
    var any1 = any(dictFoldable);
    return function(dictHeytingAlgebra) {
      return any1(dictHeytingAlgebra)(identity4);
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a3) {
      return [a3];
    }
    function array2(a3) {
      return function(b2) {
        return [a3, b2];
      };
    }
    function array3(a3) {
      return function(b2) {
        return function(c2) {
          return [a3, b2, c2];
        };
      };
    }
    function concat22(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map26) {
        return function(pure20) {
          return function(f) {
            return function(array) {
              function go(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure20([]);
                  case 1:
                    return map26(array1)(f(array[bot]));
                  case 2:
                    return apply3(map26(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map26(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply3(map26(concat22)(go(bot, pivot)))(go(pivot, top3));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value = b2;
              while (true) {
                var maybe2 = f(value);
                if (isNothing2(maybe2))
                  return result;
                var tuple2 = fromJust6(maybe2);
                result.push(fst2(tuple2));
                value = snd2(tuple2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value = b2;
              while (true) {
                var tuple2 = f(value);
                result.push(fst2(tuple2));
                var maybe2 = snd2(tuple2);
                if (isNothing2(maybe2))
                  return result;
                value = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldr1 = function(dict) {
    return dict.unfoldr1;
  };
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };
  var replicate1 = function(dictUnfoldable1) {
    var unfoldr11 = unfoldr1(dictUnfoldable1);
    return function(n) {
      return function(v) {
        var step2 = function(i) {
          if (i <= 0) {
            return new Tuple(v, Nothing.value);
          }
          ;
          if (otherwise) {
            return new Tuple(v, new Just(i - 1 | 0));
          }
          ;
          throw new Error("Failed pattern match at Data.Unfoldable1 (line 68, column 5 - line 68, column 39): " + [i.constructor.name]);
        };
        return unfoldr11(step2)(n - 1 | 0);
      };
    };
  };
  var singleton = function(dictUnfoldable1) {
    return replicate1(dictUnfoldable1)(1);
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var replicate2 = function(dictUnfoldable) {
    var unfoldr12 = unfoldr(dictUnfoldable);
    return function(n) {
      return function(v) {
        var step2 = function(i) {
          var $17 = i <= 0;
          if ($17) {
            return Nothing.value;
          }
          ;
          return new Just(new Tuple(v, i - 1 | 0));
        };
        return unfoldr12(step2)(n);
      };
    };
  };

  // output/Data.Array/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x2) {
      return function(xs) {
        return new Just({
          head: x2,
          tail: xs
        });
      };
    });
  }();
  var toUnfoldable = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(xs) {
      var len = length(xs);
      var f = function(i) {
        if (i < len) {
          return new Just(new Tuple(unsafeIndex1(xs)(i), i + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [i.constructor.name]);
      };
      return unfoldr3(f)(0);
    };
  };
  var take = function(n) {
    return function(xs) {
      var $145 = n < 1;
      if ($145) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
    });
  };
  var sort = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(xs) {
      return sortBy(compare4)(xs);
    };
  };
  var singleton2 = function(a3) {
    return [a3];
  };
  var mapWithIndex = function(f) {
    return function(xs) {
      return zipWith(f)(range(0)(length(xs) - 1 | 0))(xs);
    };
  };
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    var eq23 = eq(dictEq);
    return function(x2) {
      return findIndex(function(v) {
        return eq23(v)(x2);
      });
    };
  };
  var notElem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a3) {
      return function(arr) {
        return isNothing(elemIndex1(a3)(arr));
      };
    };
  };
  var elem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a3) {
      return function(arr) {
        return isJust(elemIndex1(a3)(arr));
      };
    };
  };
  var cons2 = function(x2) {
    return function(xs) {
      return append2([x2])(xs);
    };
  };
  var some = function(dictAlternative) {
    var apply1 = apply(dictAlternative.Applicative0().Apply0());
    var map33 = map(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer4 = defer(dictLazy);
      return function(v) {
        return apply1(map33(cons2)(v))(defer4(function(v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many = function(dictAlternative) {
    var alt7 = alt(dictAlternative.Plus1().Alt0());
    var pure111 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt7(some(dictAlternative)(dictLazy)(v))(pure111([]));
      };
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();

  // output/Data.List.Types/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map3 = /* @__PURE__ */ map(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev = function() {
          var go = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go(Nil.value);
        }();
        var $281 = foldl(foldableList)(flip(f))(b2);
        return function($282) {
          return $281(rev($282));
        };
      };
    },
    foldl: function(f) {
      var go = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $283 = append22(acc);
          return function($284) {
            return $283(f($284));
          };
        })(mempty3);
      };
    }
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var traversableList = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      var map112 = map(Apply0.Functor0());
      var lift24 = lift2(Apply0);
      var pure111 = pure(dictApplicative);
      return function(f) {
        var $298 = map112(foldl2(flip(Cons.create))(Nil.value));
        var $299 = foldl2(function(acc) {
          var $301 = lift24(flip(Cons.create))(acc);
          return function($302) {
            return $301(f($302));
          };
        })(pure111(Nil.value));
        return function($300) {
          return $298($299($300));
        };
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableList)(dictApplicative)(identity5);
    },
    Functor0: function() {
      return functorList;
    },
    Foldable1: function() {
      return foldableList;
    }
  };
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map3(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var bindList = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(v1(v.value0))(bind(bindList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 164, column 1 - line 166, column 37): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyList;
    }
  };
  var applicativeList = {
    pure: function(a3) {
      return new Cons(a3, Nil.value);
    },
    Apply0: function() {
      return applyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();
  var alternativeList = {
    Applicative0: function() {
      return applicativeList;
    },
    Plus1: function() {
      return plusList;
    }
  };

  // output/Data.List/index.js
  var map4 = /* @__PURE__ */ map(functorMaybe);
  var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var bind2 = /* @__PURE__ */ bind(bindList);
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var uncons2 = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable2 = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map4(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons2(xs));
    });
  };
  var singleton4 = function(a3) {
    return new Cons(a3, Nil.value);
  };
  var reverse2 = /* @__PURE__ */ function() {
    var go = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return acc;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_acc = new Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go(Nil.value);
  }();
  var take2 = /* @__PURE__ */ function() {
    var go = function($copy_acc) {
      return function($copy_v) {
        return function($copy_v1) {
          var $tco_var_acc = $copy_acc;
          var $tco_var_v = $copy_v;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(acc, v, v1) {
            if (v < 1) {
              $tco_done = true;
              return reverse2(acc);
            }
            ;
            if (v1 instanceof Nil) {
              $tco_done = true;
              return reverse2(acc);
            }
            ;
            if (v1 instanceof Cons) {
              $tco_var_acc = new Cons(v1.value0, acc);
              $tco_var_v = v - 1 | 0;
              $copy_v1 = v1.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List (line 513, column 3 - line 513, column 35): " + [acc.constructor.name, v.constructor.name, v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_acc, $tco_var_v, $copy_v1);
          }
          ;
          return $tco_result;
        };
      };
    };
    return go(Nil.value);
  }();
  var range3 = function(start2) {
    return function(end) {
      if (start2 === end) {
        return singleton4(start2);
      }
      ;
      if (otherwise) {
        var go = function($copy_s) {
          return function($copy_e) {
            return function($copy_step) {
              return function($copy_rest) {
                var $tco_var_s = $copy_s;
                var $tco_var_e = $copy_e;
                var $tco_var_step = $copy_step;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(s, e, step2, rest) {
                  if (s === e) {
                    $tco_done = true;
                    return new Cons(s, rest);
                  }
                  ;
                  if (otherwise) {
                    $tco_var_s = s + step2 | 0;
                    $tco_var_e = e;
                    $tco_var_step = step2;
                    $copy_rest = new Cons(s, rest);
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Data.List (line 148, column 3 - line 149, column 65): " + [s.constructor.name, e.constructor.name, step2.constructor.name, rest.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_s, $tco_var_e, $tco_var_step, $copy_rest);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
        return go(end)(start2)(function() {
          var $312 = start2 > end;
          if ($312) {
            return 1;
          }
          ;
          return -1 | 0;
        }())(Nil.value);
      }
      ;
      throw new Error("Failed pattern match at Data.List (line 144, column 1 - line 144, column 32): " + [start2.constructor.name, end.constructor.name]);
    };
  };
  var manyRec = function(dictMonadRec) {
    var bind13 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt7 = alt(Alt0);
      var map112 = map(Alt0.Functor0());
      var pure20 = pure(dictAlternative.Applicative0());
      return function(p5) {
        var go = function(acc) {
          return bind13(alt7(map112(Loop.create)(p5))(pure20(new Done(unit))))(function(aa) {
            return pure20(bimap2(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse2(acc);
            })(aa));
          });
        };
        return tailRecM4(go)(Nil.value);
      };
    };
  };
  var some2 = function(dictAlternative) {
    var apply3 = apply(dictAlternative.Applicative0().Apply0());
    var map112 = map(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer4 = defer(dictLazy);
      return function(v) {
        return apply3(map112(Cons.create)(v))(defer4(function(v1) {
          return many2(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many2 = function(dictAlternative) {
    var alt7 = alt(dictAlternative.Plus1().Alt0());
    var pure20 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt7(some2(dictAlternative)(dictLazy)(v))(pure20(Nil.value));
      };
    };
  };
  var length2 = /* @__PURE__ */ foldl3(function(acc) {
    return function(v) {
      return acc + 1 | 0;
    };
  })(0);
  var fromFoldable2 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };
  var filter2 = function(p5) {
    var go = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return reverse2(acc);
          }
          ;
          if (v instanceof Cons) {
            if (p5(v.value0)) {
              $tco_var_acc = new Cons(v.value0, acc);
              $copy_v = v.value1;
              return;
            }
            ;
            if (otherwise) {
              $tco_var_acc = acc;
              $copy_v = v.value1;
              return;
            }
            ;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 390, column 3 - line 390, column 27): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go(Nil.value);
  };
  var drop = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v < 1) {
          $tco_done = true;
          return v1;
        }
        ;
        if (v1 instanceof Nil) {
          $tco_done = true;
          return Nil.value;
        }
        ;
        if (v1 instanceof Cons) {
          $tco_var_v = v - 1 | 0;
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 536, column 1 - line 536, column 42): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var concatMap2 = /* @__PURE__ */ flip(bind2);
  var concat2 = function(v) {
    return bind2(v)(identity6);
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _12, _22, _3) {
      this.tag = tag;
      this._1 = _12;
      this._2 = _22;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_12, _22, _3) {
        return new Aff2(tag, _12, _22, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k2) {
      try {
        return eff(k2)();
      } catch (error4) {
        k2(left(error4))();
        return nonCanceler;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size3 = 0;
      var ix2 = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size3 !== 0) {
          size3--;
          thunk = queue[ix2];
          queue[ix2] = void 0;
          ix2 = (ix2 + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size3 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix2 + size3) % limit] = cb;
          size3++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k2 in fibers) {
              if (fibers.hasOwnProperty(k2)) {
                killCount++;
                kill(k2);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k3 in kills) {
                  if (kills.hasOwnProperty(k3)) {
                    kills[k3]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step2 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run4(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step2 = bhead(step2);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step2 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step2)) {
                status = RETURN;
                fail2 = step2;
                step2 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step2 = util.fromRight(step2);
              }
              break;
            case CONTINUE:
              switch (step2.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step2._2;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step2 = util.right(step2._1);
                  } else {
                    status = STEP_BIND;
                    step2 = step2._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step2 = runSync(util.left, util.right, step2._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step2 = runAsync(util.left, step2._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step2 = result2;
                        run4(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step2._1);
                  step2 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step2._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step2._1) {
                    tmp.run();
                  }
                  step2 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step2 = sequential2(util, supervisor, step2._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step2 = interrupt || fail2 || step2;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step2 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step2 = util.fromRight(step2);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step2);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step2 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step2 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step2 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step2 = attempt._1.completed(util.fromRight(step2))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step2 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step2 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k2 in joins) {
                if (joins.hasOwnProperty(k2)) {
                  rethrow = rethrow && joins[k2].rethrow;
                  runEff(joins[k2].handler(step2));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step2) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step2);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join3) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join3.rethrow;
            join3.handler(step2)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join3;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step2 = interrupt;
              run4(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error4)), attempts, interrupt);
                }
                status = RETURN;
                step2 = null;
                fail2 = null;
                run4(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step2 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join2(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run4(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join2,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run4(runTick);
              });
            } else {
              run4(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill(error4, par2, cb2) {
        var step2 = par2;
        var head4 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step2.tag) {
              case FORKED:
                if (step2._3 === EMPTY) {
                  tmp = fibers[step2._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head4 === null) {
                  break loop;
                }
                step2 = head4._2;
                if (tail2 === null) {
                  head4 = null;
                } else {
                  head4 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step2 = step2._2;
                break;
              case APPLY:
              case ALT:
                if (head4) {
                  tail2 = new Aff2(CONS, head4, tail2);
                }
                head4 = step2;
                step2 = step2._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join2(result, head4, tail2) {
        var fail2, step2, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step2 = null;
        } else {
          step2 = result;
          fail2 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head4 === null) {
              cb(fail2 || step2)();
              return;
            }
            if (head4._3 !== EMPTY) {
              return;
            }
            switch (head4.tag) {
              case MAP:
                if (fail2 === null) {
                  head4._3 = util.right(head4._1(util.fromRight(step2)));
                  step2 = head4._3;
                } else {
                  head4._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (fail2) {
                  head4._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, fail2 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join2(fail2, null, null);
                      } else {
                        join2(fail2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step2 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head4._3 = step2;
                }
                break;
              case ALT:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step2 === lhs ? rhs : lhs;
                  step2 = null;
                  head4._3 = fail2;
                } else {
                  head4._3 = step2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, step2 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join2(step2, null, null);
                      } else {
                        join2(step2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head4 = null;
            } else {
              head4 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join2(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run4() {
        var status = CONTINUE;
        var step2 = par;
        var head4 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step2.tag) {
                  case MAP:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                    step2 = step2._2;
                    break;
                  case APPLY:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  case ALT:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step2;
                    step2 = new Aff2(FORKED, fid, new Aff2(CONS, head4, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step2)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head4 === null) {
                  break loop;
                }
                if (head4._1 === EMPTY) {
                  head4._1 = step2;
                  status = CONTINUE;
                  step2 = head4._2;
                  head4._2 = EMPTY;
                } else {
                  head4._2 = step2;
                  step2 = head4;
                  if (tail2 === null) {
                    head4 = null;
                  } else {
                    head4 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step2;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler;
            };
          });
        };
      }
      run4();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k2) {
      return Aff.Catch(aff, k2);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value) {
          return Aff.Pure(f(value));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k2) {
      return Aff.Bind(aff, k2);
    };
  }
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k2) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k2);
      } else {
        return setTimeout(k2, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy3 = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var functorAff = {
    map: _map
  };
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy3("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindAff);
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k2) {
    return function(aff) {
      return launchAff(bindFlipped2(function($77) {
        return liftEffect2(k2($77));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k2) {
    return function(aff) {
      return $$void2(runAff(k2)(aff));
    };
  };

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Flame.Application.Internal.Dom/foreign.js
  function querySelector_(selector) {
    return document.querySelector(selector);
  }
  function createWindowListener_(eventName, updater) {
    window.addEventListener(eventName, function(event) {
      updater(event)();
    });
  }
  function createDocumentListener_(eventName, updater) {
    document.addEventListener(eventName, function(event) {
      updater(event)();
    });
  }
  function createCustomListener_(eventName, updater) {
    document.addEventListener(eventName, function(event) {
      updater(event.detail)();
    });
  }

  // output/Data.Nullable/foreign.js
  function nullable(a3, r2, f) {
    return a3 == null ? r2 : f(a3);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Effect.Uncurried/foreign.js
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a3) {
      return function() {
        return fn(a3);
      };
    };
  };
  var runEffectFn2 = function runEffectFn22(fn) {
    return function(a3) {
      return function(b2) {
        return function() {
          return fn(a3, b2);
        };
      };
    };
  };
  var runEffectFn4 = function runEffectFn42(fn) {
    return function(a3) {
      return function(b2) {
        return function(c2) {
          return function(d) {
            return function() {
              return fn(a3, b2, c2, d);
            };
          };
        };
      };
    };
  };

  // output/Flame.Application.Internal.Dom/index.js
  var querySelector = function(selector) {
    return function __do() {
      var selected = querySelector_(selector);
      return toMaybe(selected);
    };
  };
  var createWindowListener = /* @__PURE__ */ runEffectFn2(createWindowListener_);
  var createDocumentListener = /* @__PURE__ */ runEffectFn2(createDocumentListener_);
  var createCustomListener = /* @__PURE__ */ runEffectFn2(createCustomListener_);

  // output/Data.String.Regex/foreign.js
  var regexImpl = function(left) {
    return function(right) {
      return function(s1) {
        return function(s2) {
          try {
            return right(new RegExp(s1, s2));
          } catch (e) {
            return left(e.message);
          }
        };
      };
    };
  };
  var _replaceBy = function(just) {
    return function(nothing) {
      return function(r2) {
        return function(f) {
          return function(s) {
            return s.replace(r2, function(match3) {
              var groups = [];
              var group3, i = 1;
              while (typeof (group3 = arguments[i++]) !== "number") {
                groups.push(group3 == null ? nothing : just(group3));
              }
              return f(match3)(groups);
            });
          };
        };
      };
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a3) {
    return a3.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton5 = function(c2) {
    return c2;
  };
  var _toChar = function(just) {
    return function(nothing) {
      return function(s) {
        return s.length === 1 ? just(s) : nothing;
      };
    };
  };
  var length3 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i) {
    return function(s) {
      return { before: s.substring(0, i), after: s.substring(i) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var uncons3 = function(v) {
    if (v === "") {
      return Nothing.value;
    }
    ;
    return new Just({
      head: charAt(0)(v),
      tail: drop2(1)(v)
    });
  };
  var toChar = /* @__PURE__ */ function() {
    return _toChar(Just.create)(Nothing.value);
  }();
  var stripSuffix = function(v) {
    return function(str) {
      var v1 = splitAt(length3(str) - length3(v) | 0)(str);
      var $14 = v1.after === v;
      if ($14) {
        return new Just(v1.before);
      }
      ;
      return Nothing.value;
    };
  };
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length3(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };

  // output/Control.Alternative/index.js
  var guard = function(dictAlternative) {
    var pure20 = pure(dictAlternative.Applicative0());
    var empty5 = empty(dictAlternative.Plus1());
    return function(v) {
      if (v) {
        return pure20(unit);
      }
      ;
      if (!v) {
        return empty5;
      }
      ;
      throw new Error("Failed pattern match at Control.Alternative (line 48, column 1 - line 48, column 54): " + [v.constructor.name]);
    };
  };

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toLower = function(s) {
    return s.toLowerCase();
  };
  var toUpper = function(s) {
    return s.toUpperCase();
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Data.String.Regex.Flags/index.js
  var global = {
    global: true,
    ignoreCase: false,
    multiline: false,
    dotAll: false,
    sticky: false,
    unicode: false
  };

  // output/Data.String.Regex/index.js
  var replace$prime = /* @__PURE__ */ function() {
    return _replaceBy(Just.create)(Nothing.value);
  }();
  var renderFlags = function(v) {
    return function() {
      if (v.global) {
        return "g";
      }
      ;
      return "";
    }() + (function() {
      if (v.ignoreCase) {
        return "i";
      }
      ;
      return "";
    }() + (function() {
      if (v.multiline) {
        return "m";
      }
      ;
      return "";
    }() + (function() {
      if (v.dotAll) {
        return "s";
      }
      ;
      return "";
    }() + (function() {
      if (v.sticky) {
        return "y";
      }
      ;
      return "";
    }() + function() {
      if (v.unicode) {
        return "u";
      }
      ;
      return "";
    }()))));
  };
  var regex = function(s) {
    return function(f) {
      return regexImpl(Left.create)(Right.create)(s)(renderFlags(f));
    };
  };

  // output/Flame.Html.Attribute.Internal/foreign.js
  var styleData = 1;
  var classData = 2;
  var propertyData = 3;
  function createProperty_(name2) {
    return function(value) {
      return [propertyData, name2, value];
    };
  }
  function createClass(array) {
    return [classData, array];
  }
  function createStyle(object) {
    return [styleData, object];
  }

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index3) {
            return function(str) {
              var length5 = str.length;
              if (index3 < 0 || index3 >= length5)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i = index3; ; --i) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index3)(str);
            };
          };
        };
      };
    };
  };
  var _fromCodePointArray = function(singleton10) {
    return hasFromCodePoint ? function(cps) {
      if (cps.length < 1e4) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton10).join("");
    } : function(cps) {
      return cps.map(singleton10).join("");
    };
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c2) {
    return c2.charCodeAt(0);
  }
  function fromCharCode(c2) {
    return String.fromCharCode(c2);
  }

  // output/Data.Enum/index.js
  var bind3 = /* @__PURE__ */ bind(bindMaybe);
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorMaybe);
  var guard2 = /* @__PURE__ */ guard(alternativeMaybe);
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var succ = function(dict) {
    return dict.succ;
  };
  var pred = function(dict) {
    return dict.pred;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low) {
      return function(high) {
        return function(x2) {
          var v = toEnum1(x2);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x2 < fromEnum1(bottom22);
            if ($140) {
              return low;
            }
            ;
            return high;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var enumFromTo = function(dictEnum) {
    var Ord0 = dictEnum.Ord0();
    var eq13 = eq(Ord0.Eq0());
    var lessThan1 = lessThan(Ord0);
    var succ1 = succ(dictEnum);
    var lessThanOrEq1 = lessThanOrEq(Ord0);
    var pred1 = pred(dictEnum);
    var greaterThanOrEq1 = greaterThanOrEq(Ord0);
    return function(dictUnfoldable1) {
      var singleton10 = singleton(dictUnfoldable1);
      var unfoldr12 = unfoldr1(dictUnfoldable1);
      var go = function(step2) {
        return function(op2) {
          return function(to3) {
            return function(a3) {
              return new Tuple(a3, bind3(step2(a3))(function(a$prime) {
                return voidLeft2(guard2(op2(a$prime)(to3)))(a$prime);
              }));
            };
          };
        };
      };
      return function(v) {
        return function(v1) {
          if (eq13(v)(v1)) {
            return singleton10(v);
          }
          ;
          if (lessThan1(v)(v1)) {
            return unfoldr12(go(succ1)(lessThanOrEq1)(v1))(v);
          }
          ;
          if (otherwise) {
            return unfoldr12(go(pred1)(greaterThanOrEq1)(v1))(v);
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 186, column 14 - line 190, column 51): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a3) {
        return toEnum$prime(fromEnum$prime(a3) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a3) {
        return toEnum$prime(fromEnum$prime(a3) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;
  var pow = function(n) {
    return function(p5) {
      return Math.pow(n, p5);
    };
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x2) {
    if (!isFiniteImpl(x2)) {
      return 0;
    }
    ;
    if (x2 >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x2 <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x2));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x2.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy4 = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x2) {
    return x2;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons4 = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $42 = isLead(cu0) && isTrail(cu1);
    if ($42) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map5(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons4(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $46 = isLead(cu0) && length3(s) > 1;
    if ($46) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $47 = isTrail(cu1);
      if ($47) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length4 = function($73) {
    return length(toCodePointArray($73));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $74 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($75) {
      return singleton5($74($75));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
  var singleton6 = /* @__PURE__ */ _singleton(singletonFallback);
  var eqCodePoint = {
    eq: function(x2) {
      return function(y) {
        return x2 === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x2) {
      return function(y) {
        return compare2(x2)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var codePointFromChar = function($76) {
    return CodePoint(fromEnum2($76));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons4(s);
        if (v instanceof Just) {
          var $65 = n === 0;
          if ($65) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy4("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Foreign.Object/foreign.js
  function runST(f) {
    return f();
  }
  function toArrayWithKey(f) {
    return function(m2) {
      var r2 = [];
      for (var k2 in m2) {
        if (hasOwnProperty.call(m2, k2)) {
          r2.push(f(k2)(m2[k2]));
        }
      }
      return r2;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k2) {
    return function() {
      return k2;
    };
  });

  // output/Data.Function.Uncurried/foreign.js
  var mkFn5 = function(fn) {
    return function(a3, b2, c2, d, e) {
      return fn(a3)(b2)(c2)(d)(e);
    };
  };

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };
  function poke2(k2) {
    return function(v) {
      return function(m2) {
        return function() {
          m2[k2] = v;
          return m2;
        };
      };
    };
  }

  // output/Foreign.Object/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var toUnfoldable3 = function(dictUnfoldable) {
    var $86 = toUnfoldable(dictUnfoldable);
    var $87 = toArrayWithKey(Tuple.create);
    return function($88) {
      return $86($87($88));
    };
  };
  var fromHomogeneous = function() {
    return unsafeCoerce2;
  };
  var fromFoldable3 = function(dictFoldable) {
    var fromFoldable12 = fromFoldable(dictFoldable);
    return function(l2) {
      return runST(function __do() {
        var s = newImpl();
        foreach(fromFoldable12(l2))(function(v) {
          return $$void3(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
    };
  };

  // output/Flame.Html.Attribute.Internal/index.js
  var fromHomogeneous2 = /* @__PURE__ */ fromHomogeneous();
  var fromJust4 = /* @__PURE__ */ fromJust();
  var crashWith3 = /* @__PURE__ */ crashWith();
  var show2 = /* @__PURE__ */ show(showString);
  var map6 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var fromFoldable4 = /* @__PURE__ */ fromFoldable3(foldableArray);
  var toStyleList = function(dict) {
    return dict.toStyleList;
  };
  var to2 = function(dict) {
    return dict.to;
  };
  var style = function(dictToStyleList) {
    var toStyleList1 = toStyleList(dictToStyleList);
    return function(record) {
      return createStyle(toStyleList1(record));
    };
  };
  var stringClassList = {
    to: /* @__PURE__ */ function() {
      var $41 = filter(function() {
        var $44 = not(heytingAlgebraBoolean);
        return function($45) {
          return $44($$null2($45));
        };
      }());
      var $42 = split(" ");
      return function($43) {
        return $41($42($43));
      };
    }()
  };
  var createProperty = function(name1) {
    return function(value1) {
      return createProperty_(name1)(value1);
    };
  };
  var id = /* @__PURE__ */ createProperty("id");
  var placeholder = /* @__PURE__ */ createProperty("placeholder");
  var type$prime = /* @__PURE__ */ createProperty("type");
  var caseify = function(name$prime) {
    if (name$prime === toUpper(name$prime)) {
      return toLower(name$prime);
    }
    ;
    if (otherwise) {
      var v = fromJust4(uncons4(name$prime));
      var replacer = function($128) {
        return $$const(function(v1) {
          return "-" + v1;
        }(toLower($128)));
      };
      var regex3 = function() {
        var v1 = regex("[A-Z]")(global);
        if (v1 instanceof Right) {
          return v1.value0;
        }
        ;
        if (v1 instanceof Left) {
          return crashWith3(show2(v1.value0));
        }
        ;
        throw new Error("Failed pattern match at Flame.Html.Attribute.Internal (line 98, column 38 - line 100, column 56): " + [v1.constructor.name]);
      }();
      var hyphenated = replace$prime(regex3)(replacer)(v.tail);
      return toLower(singleton6(v.head)) + hyphenated;
    }
    ;
    throw new Error("Failed pattern match at Flame.Html.Attribute.Internal (line 92, column 1 - line 92, column 28): " + [name$prime.constructor.name]);
  };
  var class$prime = function(dictToClassList) {
    var $129 = map6(caseify);
    var $130 = to2(dictToClassList);
    return function($131) {
      return createClass($129($130($131)));
    };
  };
  var recordStyleList = function() {
    return {
      toStyleList: function() {
        var toArray3 = function($132) {
          return toUnfoldable4(fromHomogeneous2($132));
        };
        var go = function(v) {
          return new Tuple(caseify(v.value0), v.value1);
        };
        var $133 = map6(go);
        return function($134) {
          return fromFoldable4($133(toArray3($134)));
        };
      }()
    };
  };
  var booleanToFalsyString = function(v) {
    if (v) {
      return "true";
    }
    ;
    if (!v) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Flame.Html.Attribute.Internal (line 75, column 7 - line 77, column 24): " + [v.constructor.name]);
  };
  var checked = /* @__PURE__ */ function() {
    var $135 = createProperty("checked");
    return function($136) {
      return $135(booleanToFalsyString($136));
    };
  }();

  // output/Flame.Html.Element/foreign.js
  var textNode = 1;
  var elementNode = 2;
  var svgNode = 3;
  var styleData2 = 1;
  var classData2 = 2;
  var propertyData2 = 3;
  var attributeData = 4;
  var keyData = 7;
  function createElementNode(tag) {
    return function(nodeData) {
      return function(potentialChildren) {
        let children = potentialChildren, text4 = void 0;
        if (potentialChildren.length === 1 && potentialChildren[0].nodeType == textNode) {
          children = void 0;
          text4 = potentialChildren[0].text;
        }
        return {
          nodeType: elementNode,
          node: void 0,
          tag,
          nodeData: fromNodeData(nodeData),
          children,
          text: text4
        };
      };
    };
  }
  function createDatalessElementNode(tag) {
    return function(potentialChildren) {
      let children = potentialChildren, text4 = void 0;
      if (potentialChildren.length === 1 && potentialChildren[0].nodeType == textNode) {
        children = void 0;
        text4 = potentialChildren[0].text;
      }
      return {
        nodeType: elementNode,
        node: void 0,
        tag,
        nodeData: {},
        children,
        text: text4
      };
    };
  }
  function createSingleElementNode(tag) {
    return function(nodeData) {
      return {
        nodeType: elementNode,
        node: void 0,
        tag,
        nodeData: fromNodeData(nodeData)
      };
    };
  }
  function createEmptyElement(tag) {
    return {
      nodeType: tag.trim().toLowerCase() === "svg" ? svgNode : elementNode,
      node: void 0,
      tag,
      nodeData: {}
    };
  }
  function text(value) {
    return {
      nodeType: textNode,
      node: void 0,
      text: value
    };
  }
  function fromNodeData(allData) {
    let nodeData = {};
    if (allData !== void 0)
      for (let data of allData) {
        let dataOne = data[1];
        switch (data[0]) {
          case styleData2:
            if (nodeData.styles === void 0)
              nodeData.styles = {};
            for (let key2 in dataOne)
              nodeData.styles[key2] = dataOne[key2];
            break;
          case classData2:
            if (nodeData.classes === void 0)
              nodeData.classes = [];
            nodeData.classes = nodeData.classes.concat(dataOne);
            break;
          case propertyData2:
            if (nodeData.properties === void 0)
              nodeData.properties = {};
            nodeData.properties[dataOne] = data[2];
            break;
          case attributeData:
            if (nodeData.attributes === void 0)
              nodeData.attributes = {};
            nodeData.attributes[dataOne] = data[2];
            break;
          case keyData:
            nodeData.key = dataOne;
            break;
          default:
            if (nodeData.events === void 0)
              nodeData.events = {};
            if (nodeData.events[dataOne] === void 0)
              nodeData.events[dataOne] = [];
            nodeData.events[dataOne].push(data[2]);
        }
      }
    return nodeData;
  }

  // output/Flame.Html.Element/index.js
  var toNode = function(dict) {
    return dict.toNode;
  };
  var stringToNodeData = {
    toNode: function($767) {
      return singleton2(id($767));
    }
  };
  var stringToHtml = {
    toNode: function($768) {
      return singleton2(text($768));
    }
  };
  var nodeDataToNodedata = {
    toNode: singleton2
  };
  var htmlToHtml = {
    toNode: singleton2
  };
  var createElement_ = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(children) {
        return createDatalessElementNode(tag)(toNode1(children));
      };
    };
  };
  var div_ = function(dictToNode) {
    return createElement_("div")(dictToNode);
  };
  var li_ = function(dictToNode) {
    return createElement_("li")(dictToNode);
  };
  var span_ = function(dictToNode) {
    return createElement_("span")(dictToNode);
  };
  var strong_ = function(dictToNode) {
    return createElement_("strong")(dictToNode);
  };
  var ul_ = function(dictToNode) {
    return createElement_("ul")(dictToNode);
  };
  var createElement$prime = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(nodeData) {
        return createSingleElementNode(tag)(toNode1(nodeData));
      };
    };
  };
  var input = function(dictToNode) {
    return createElement$prime("input")(dictToNode);
  };
  var createElement = function(tag) {
    return function(dictToNode) {
      var toNode1 = toNode(dictToNode);
      return function(dictToNode1) {
        var toNode2 = toNode(dictToNode1);
        return function(nodeData) {
          return function(children) {
            return createElementNode(tag)(toNode1(nodeData))(toNode2(children));
          };
        };
      };
    };
  };
  var div3 = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("div")(dictToNode)(dictToNode1);
    };
  };
  var li = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("li")(dictToNode)(dictToNode1);
    };
  };
  var p = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("p")(dictToNode)(dictToNode1);
    };
  };
  var span = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("span")(dictToNode)(dictToNode1);
    };
  };
  var ul = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("ul")(dictToNode)(dictToNode1);
    };
  };
  var button = function(dictToNode) {
    return function(dictToNode1) {
      return createElement("button")(dictToNode)(dictToNode1);
    };
  };
  var br = /* @__PURE__ */ createEmptyElement("br");
  var arrayToNodeData = function(dictToNode) {
    return {
      toNode: concatMap(toNode(dictToNode))
    };
  };

  // output/Flame.Renderer.String/foreign.js
  var reUnescapedHtml = /[&<>"']/g;
  var reHasUnescapedHtml = RegExp(reUnescapedHtml.source);

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x2) {
      return function(xs) {
        return new ConsCell(x2, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply3) {
      return function(map26) {
        return function(f) {
          var buildFrom = function(x2, ys) {
            return apply3(map26(consList)(f(x2)))(ys);
          };
          var go = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map26(finalCell)(f(array[array.length - 1]));
            var result = go(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map26(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.List.NonEmpty/index.js
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var cons$prime = function(x2) {
    return function(xs) {
      return new NonEmpty(x2, xs);
    };
  };

  // output/Data.Lazy/foreign.js
  var defer2 = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l2) {
    return l2();
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf3() {
    }
    ;
    Leaf3.value = new Leaf3();
    return Leaf3;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Two2(value0, value1, value2, value3);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return new Three2(value0, value1, value2, value3, value4, value5, value6);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoLeft2(value0, value1, value2);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoRight2(value0, value1, value2);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeLeft2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeMiddle2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeRight2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new KickUp2(value0, value1, value2, value3);
          };
        };
      };
    };
    return KickUp2;
  }();
  var lookup2 = function(dictOrd) {
    var compare4 = compare(dictOrd);
    return function(k2) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare4(k2)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare4(k2)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare4(k2)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert2 = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare4 = compare(dictOrd);
    return function(k2) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k2, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = compare4(k2)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Two(v1.value0, k2, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = compare4(k2)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, k2, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = compare4(k2)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k2, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var empty3 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();

  // output/Flame.Internal.Equality/foreign.js
  function compareReference_(a3) {
    return function(b2) {
      return a3 === b2;
    };
  }

  // output/Flame.Internal.Equality/index.js
  var compareReference = function(a3) {
    return function(a22) {
      return compareReference_(a3)(a22);
    };
  };
  var modelHasChanged = function(old) {
    return function($$new2) {
      return !compareReference(old)($$new2);
    };
  };

  // output/Flame.Renderer.Internal.Dom/foreign.js
  var namespace = "http://www.w3.org/2000/svg";
  var eventPrefix = "__flame_";
  var eventPostfix = "updater";
  var textNode2 = 1;
  var elementNode2 = 2;
  var svgNode2 = 3;
  var fragmentNode = 4;
  var lazyNode = 5;
  var managedNode = 6;
  var nonBubblingEvents = ["focus", "blur", "scroll"];
  function start_(eventWrapper, root, updater, html) {
    return new F(eventWrapper, root, updater, html, false);
  }
  function startFrom_(eventWrapper, root, updater, html) {
    return new F(eventWrapper, root, updater, html, true);
  }
  function resume_(f, html) {
    f.resume(html);
  }
  function F(eventWrapper, root, updater, html, isDry) {
    this.eventWrapper = eventWrapper;
    this.applicationEvents = /* @__PURE__ */ new Map();
    this.root = root;
    this.updater = updater;
    this.cachedHtml = html.node === void 0 ? html : shallowCopy(html);
    if (isDry)
      this.hydrate(this.root, this.cachedHtml);
    else
      this.createAllNodes(this.root, this.cachedHtml);
  }
  F.prototype.hydrate = function(parent, html, referenceNode) {
    switch (html.nodeType) {
      case lazyNode:
        html.node = parent;
        html.rendered = html.render(html.arg);
        html.render = void 0;
        this.hydrate(parent, html.rendered);
        break;
      case textNode2:
        html.node = parent;
        break;
      case managedNode:
        this.createAllNodes(parent, html, referenceNode);
        break;
      default:
        if (html.nodeType === fragmentNode)
          html.node = document.createDocumentFragment();
        else {
          html.node = parent;
          if (html.nodeData.events !== void 0)
            this.createAllEvents(parent, html);
        }
        let htmlChildrenLength;
        if (html.text === void 0 && html.children !== void 0 && (htmlChildrenLength = html.children.length) > 0) {
          let childNodes = parent.childNodes;
          for (let i = 0, cni = 0; i < htmlChildrenLength; ++i, ++cni) {
            let c2 = html.children[i] = html.children[i].node === void 0 ? html.children[i] : shallowCopy(html.children[i]);
            if (childNodes[cni] === void 0)
              this.createAllNodes(parent, c2);
            else {
              if (c2.nodeType === fragmentNode) {
                let fragmentChildrenLength = c2.children.length;
                c2.node = document.createDocumentFragment();
                for (let j = 0; j < fragmentChildrenLength; ++j) {
                  let cf = c2.children[j] = c2.children[j].node === void 0 ? c2.children[j] : shallowCopy(c2.children[j]);
                  this.hydrate(childNodes[cni++], cf);
                }
                cni--;
              } else if (c2.nodeType === managedNode)
                this.hydrate(parent, c2, childNodes[cni]);
              else
                this.hydrate(childNodes[cni], c2);
            }
          }
        }
    }
  };
  function shallowCopy(origin) {
    switch (origin.nodeType) {
      case textNode2:
        return {
          nodeType: textNode2,
          node: void 0,
          text: origin.text
        };
      case fragmentNode:
        return {
          nodeType: fragmentNode,
          node: void 0,
          children: origin.children
        };
      case lazyNode:
        return {
          nodeType: lazyNode,
          node: void 0,
          nodeData: origin.nodeData,
          render: origin.render,
          arg: origin.arg,
          rendered: void 0,
          messageMapper: origin.messageMapper
        };
      case managedNode:
        return {
          nodeType: managedNode,
          node: void 0,
          nodeData: origin.nodeData,
          createNode: origin.createNode,
          updateNode: origin.updateNode,
          arg: origin.arg,
          messageMapper: origin.messageMapper
        };
      default:
        return {
          nodeType: origin.nodeType,
          node: void 0,
          tag: origin.tag,
          nodeData: origin.nodeData,
          children: origin.children,
          text: origin.text,
          messageMapper: origin.messageMapper
        };
    }
  }
  F.prototype.createAllNodes = function(parent, html, referenceNode) {
    let node = this.createNode(html);
    if (html.text !== void 0)
      node.textContent = html.text;
    else {
      if (html.children !== void 0)
        this.createChildrenNodes(node, html.children);
      else if (html.rendered !== void 0) {
        if (html.messageMapper !== void 0)
          lazyMessageMap(html.messageMapper, html.rendered);
        if (html.rendered.text !== void 0) {
          node.textContent = html.rendered.text;
        } else if (html.rendered.children !== void 0)
          this.createChildrenNodes(node, html.rendered.children);
      }
    }
    parent.insertBefore(node, referenceNode);
  };
  F.prototype.checkCreateAllNodes = function(parent, html, referenceNode) {
    if (html.node !== void 0)
      html = shallowCopy(html);
    this.createAllNodes(parent, html, referenceNode);
    return html;
  };
  F.prototype.createChildrenNodes = function(parent, children) {
    let childrenLength = children.length;
    for (let i = 0; i < childrenLength; ++i) {
      let c2 = children[i] = children[i].node === void 0 ? children[i] : shallowCopy(children[i]), node = this.createNode(c2);
      if (c2.text !== void 0)
        node.textContent = c2.text;
      else {
        if (c2.children !== void 0)
          this.createChildrenNodes(node, c2.children);
        else if (c2.rendered !== void 0) {
          if (c2.messageMapper !== void 0)
            lazyMessageMap(c2.messageMapper, c2.rendered);
          if (c2.rendered.children !== void 0)
            this.createChildrenNodes(node, c2.rendered.children);
        }
      }
      parent.appendChild(node);
    }
  };
  F.prototype.createNode = function(html) {
    switch (html.nodeType) {
      case lazyNode:
        html.rendered = html.render(html.arg);
        html.render = void 0;
        return html.node = this.createNode(html.rendered);
      case textNode2:
        return html.node = document.createTextNode(html.text);
      case elementNode2:
        return html.node = this.createElement(html);
      case svgNode2:
        return html.node = this.createSvg(html);
      case fragmentNode:
        return html.node = document.createDocumentFragment();
      case managedNode:
        return html.node = this.createManagedNode(html);
    }
  };
  F.prototype.createElement = function(html) {
    let element = document.createElement(html.tag);
    this.createNodeData(element, html, false);
    return element;
  };
  F.prototype.createSvg = function(html) {
    let svg = document.createElementNS(namespace, html.tag);
    this.createNodeData(svg, html, true);
    return svg;
  };
  F.prototype.createManagedNode = function(html) {
    let node = html.createNode(html.arg)();
    html.createNode = void 0;
    this.createNodeData(node, html, node instanceof SVGElement || node.nodeName.toLowerCase() === "svg");
    return node;
  };
  F.prototype.createNodeData = function(node, html, isSvg) {
    if (html.nodeData.styles !== void 0)
      createStyles(node, html.nodeData.styles);
    if (html.nodeData.classes !== void 0 && html.nodeData.classes.length > 0)
      createClasses(node, html.nodeData.classes, isSvg);
    if (html.nodeData.attributes !== void 0)
      createAttributes(node, html.nodeData.attributes);
    if (html.nodeData.properties !== void 0)
      for (let key2 in html.nodeData.properties)
        node[key2] = html.nodeData.properties[key2];
    if (html.nodeData.events !== void 0)
      this.createAllEvents(node, html);
  };
  function createStyles(node, styles) {
    for (let key2 in styles)
      node.style.setProperty(key2, styles[key2]);
  }
  function createClasses(node, classes, isSvg) {
    let joined = classes.join(" ");
    if (isSvg)
      node.setAttribute("class", joined);
    else
      node.className = joined;
  }
  function createAttributes(node, attributes) {
    for (let key2 in attributes)
      node.setAttribute(key2, attributes[key2]);
  }
  F.prototype.createAllEvents = function(node, html) {
    for (let key2 in html.nodeData.events)
      this.createEvent(node, key2, html);
  };
  F.prototype.createEvent = function(node, name2, html) {
    let handlers = html.nodeData.events[name2], eventKey = eventPrefix + name2;
    if (nonBubblingEvents.includes(name2)) {
      let runNonBubblingEvent = this.runNonBubblingEvent(handlers, html.messageMapper);
      node[eventKey] = runNonBubblingEvent;
      node.addEventListener(name2, runNonBubblingEvent, false);
    } else {
      node[eventKey] = handlers;
      if (html.messageMapper !== void 0)
        node[eventKey + eventPostfix] = html.messageMapper;
      let synthethic = this.applicationEvents.get(name2);
      if (synthethic === void 0) {
        let runEvent = this.runEvent.bind(this);
        this.root.addEventListener(name2, runEvent, false);
        this.applicationEvents.set(name2, {
          count: 1,
          handler: runEvent
        });
      } else
        synthethic.count++;
    }
  };
  F.prototype.runNonBubblingEvent = function(handlers, messageMapper2) {
    return function(event) {
      this.runHandlers(handlers, messageMapper2, event);
    }.bind(this);
  };
  F.prototype.runEvent = function(event) {
    let node = event.target, eventKey = eventPrefix + event.type;
    while (node !== this.root) {
      let handlers = node[eventKey];
      if (handlers !== void 0) {
        this.runHandlers(handlers, node[eventKey + eventPostfix], event);
        return;
      }
      node = node.parentNode;
    }
  };
  F.prototype.runHandlers = function(handlers, messageMapper2, event) {
    let handlersLength = handlers.length;
    for (let i = 0; i < handlersLength; ++i) {
      let h = handlers[i], maybeMessage = typeof h === "function" ? h(event)() : this.eventWrapper(h);
      this.updater(messageMapper2 === void 0 ? maybeMessage : messageMapper2(maybeMessage))();
    }
    event.stopPropagation();
  };
  F.prototype.resume = function(updatedHtml) {
    this.cachedHtml = this.updateAllNodes(this.root, this.cachedHtml, updatedHtml);
    ;
  };
  F.prototype.updateAllNodes = function(parent, currentHtml2, updatedHtml) {
    if (updatedHtml.node !== void 0)
      updatedHtml = shallowCopy(updatedHtml);
    if (currentHtml2.tag !== updatedHtml.tag || currentHtml2.nodeType !== updatedHtml.nodeType) {
      this.createAllNodes(parent, updatedHtml, currentHtml2.node);
      parent.removeChild(currentHtml2.node);
    } else {
      updatedHtml.node = currentHtml2.node;
      switch (updatedHtml.nodeType) {
        case lazyNode:
          if (updatedHtml.arg !== currentHtml2.arg) {
            updatedHtml.rendered = updatedHtml.render(updatedHtml.arg);
            if (updatedHtml.messageMapper !== void 0)
              lazyMessageMap(updatedHtml.messageMapper, updatedHtml.rendered);
            this.updateAllNodes(parent, currentHtml2.rendered, updatedHtml.rendered);
          } else
            updatedHtml.rendered = currentHtml2.rendered;
          updatedHtml.render = void 0;
          break;
        case managedNode:
          let node = updatedHtml.updateNode(currentHtml2.node)(currentHtml2.arg)(updatedHtml.arg)(), isSvg = node instanceof SVGElement || node.nodeName.toLowerCase() === "svg";
          if (node !== currentHtml2.node || node.nodeType !== currentHtml2.node.nodeType || node.nodeName !== currentHtml2.node.nodeName) {
            this.createNodeData(node, updatedHtml, isSvg);
            parent.insertBefore(node, currentHtml2.node);
            parent.removeChild(currentHtml2.node);
          } else
            this.updateNodeData(node, currentHtml2.nodeData, updatedHtml, isSvg);
          updatedHtml.node = node;
          break;
        case textNode2:
          if (updatedHtml.text !== currentHtml2.text)
            updatedHtml.node.textContent = updatedHtml.text;
          break;
        case fragmentNode:
          this.updateChildrenNodes(parent, currentHtml2, updatedHtml);
          break;
        default:
          this.updateNodeData(currentHtml2.node, currentHtml2.nodeData, updatedHtml, updatedHtml.nodeType == svgNode2);
          if ((updatedHtml.text !== void 0 || updatedHtml.children === void 0 && currentHtml2.text != void 0) && !hasInnerHtml(updatedHtml.nodeData) && updatedHtml.text != currentHtml2.node.textContent)
            currentHtml2.node.textContent = updatedHtml.text;
          else
            this.updateChildrenNodes(currentHtml2.node, currentHtml2, updatedHtml);
      }
    }
    return updatedHtml;
  };
  function clearNode(node) {
    node.textContent = "";
  }
  F.prototype.updateChildrenNodes = function(parent, currentHtml2, updatedHtml) {
    let currentChildren = currentHtml2.children, updatedChildren = updatedHtml.children;
    if (currentChildren === void 0 || currentChildren.length === 0) {
      let updatedChildrenLength;
      if (updatedChildren !== void 0 && (updatedChildrenLength = updatedChildren.length) > 0) {
        if (currentHtml2.text !== void 0 || hasInnerHtml(currentHtml2.nodeData))
          clearNode(parent);
        for (let i = 0; i < updatedChildrenLength; ++i)
          updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
      }
    } else if (updatedChildren === void 0 || updatedChildren.length === 0) {
      if (currentChildren !== void 0 && (currentChildren.length > 0 || currentHtml2.text !== void 0) && !hasInnerHtml(updatedHtml.nodeData))
        clearNode(parent);
    } else if (currentChildren[0].nodeData !== void 0 && currentChildren[0].nodeData.key !== void 0 && updatedChildren[0].nodeData !== void 0 && updatedChildren[0].nodeData.key !== void 0)
      this.updateKeyedChildrenNodes(parent, currentChildren, updatedChildren);
    else
      this.updateNonKeyedChildrenNodes(parent, currentChildren, updatedChildren);
  };
  function hasInnerHtml(parentNodeData) {
    return parentNodeData !== void 0 && parentNodeData.properties !== void 0 && parentNodeData.properties.innerHTML !== void 0;
  }
  F.prototype.updateKeyedChildrenNodes = function(parent, currentChildren, updatedChildren) {
    let currentStart = 0, updatedStart = 0, currentEnd = currentChildren.length - 1, updatedEnd = updatedChildren.length - 1;
    let afterNode, currentStartNode = currentChildren[currentStart].node, updatedStartNode = currentStartNode, currentEndNode = currentChildren[currentEnd].node;
    let loop = true;
    fixes:
      while (loop) {
        loop = false;
        let currentHtml2 = currentChildren[currentStart], updatedHtml = updatedChildren[updatedStart];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          updatedStartNode = currentStartNode = currentHtml2.node.nextSibling;
          currentStart++;
          updatedStart++;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentStart];
          updatedHtml = updatedChildren[updatedStart];
        }
        currentHtml2 = currentChildren[currentEnd];
        updatedHtml = updatedChildren[updatedEnd];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          afterNode = currentEndNode;
          currentEndNode = currentEndNode.previousSibling;
          currentEnd--;
          updatedEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentEnd];
          updatedHtml = updatedChildren[updatedEnd];
        }
        currentHtml2 = currentChildren[currentEnd];
        updatedHtml = updatedChildren[updatedStart];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          loop = true;
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          currentEndNode = currentHtml2.node.previousSibling;
          parent.insertBefore(currentHtml2.node, updatedStartNode);
          updatedStart++;
          currentEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentEnd];
          updatedHtml = updatedChildren[updatedStart];
        }
        currentHtml2 = currentChildren[currentStart];
        updatedHtml = updatedChildren[updatedEnd];
        while (currentHtml2.nodeData.key === updatedHtml.nodeData.key) {
          loop = true;
          updatedHtml = this.updateAllNodes(parent, currentHtml2, updatedHtml);
          parent.insertBefore(currentHtml2.node, afterNode);
          afterNode = currentHtml2.node;
          currentStart++;
          updatedEnd--;
          if (currentEnd < currentStart || updatedEnd < updatedStart)
            break fixes;
          currentHtml2 = currentChildren[currentStart];
          updatedHtml = updatedChildren[updatedEnd];
        }
      }
    if (updatedEnd < updatedStart)
      while (currentStart <= currentEnd) {
        parent.removeChild(currentChildren[currentEnd].node);
        currentEnd--;
      }
    else if (currentEnd < currentStart)
      while (updatedStart <= updatedEnd) {
        updatedChildren[updatedStart] = this.checkCreateAllNodes(parent, updatedChildren[updatedStart], afterNode);
        updatedStart++;
      }
    else {
      let P = new Int32Array(updatedEnd + 1 - updatedStart);
      let I = /* @__PURE__ */ new Map();
      for (let i = updatedStart; i <= updatedEnd; i++) {
        P[i] = -1;
        I.set(updatedChildren[i].nodeData.key, i);
      }
      let reusingNodes = updatedStart + updatedChildren.length - 1 - updatedEnd, toRemove = [];
      for (let i = currentStart; i <= currentEnd; i++)
        if (I.has(currentChildren[i].nodeData.key)) {
          P[I.get(currentChildren[i].nodeData.key)] = i;
          reusingNodes++;
        } else
          toRemove.push(i);
      if (reusingNodes === 0) {
        parent.textContent = "";
        for (let i = updatedStart; i <= updatedEnd; i++)
          updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
      } else {
        let toRemoveLength = toRemove.length;
        for (let i = 0; i < toRemoveLength; i++)
          parent.removeChild(currentChildren[toRemove[i]].node);
        let longestSeq = longestSubsequence(P, updatedStart), seqIndex = longestSeq.length - 1;
        for (let i = updatedEnd; i >= updatedStart; i--) {
          if (longestSeq[seqIndex] === i) {
            currentHtml = currentChildren[P[longestSeq[seqIndex]]];
            updatedChildren[i] = this.updateAllNodes(parent, currentHtml, updatedChildren[i]);
            afterNode = currentHtml.node;
            seqIndex--;
          } else {
            if (P[i] === -1) {
              updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i], afterNode);
              afterNode = updatedChildren[i].node;
            } else {
              currentHtml = currentChildren[P[i]];
              updatedChildren[i] = this.updateAllNodes(parent, currentHtml, updatedChildren[i]);
              parent.insertBefore(currentHtml.node, afterNode);
              afterNode = currentHtml.node;
            }
          }
        }
      }
    }
  };
  function longestSubsequence(ns, updatedStart) {
    let seq = [], is = [], l2 = -1, i, len, pre = new Int32Array(ns.length);
    for (i = updatedStart, len = ns.length; i < len; i++) {
      let n = ns[i];
      if (n < 0)
        continue;
      let j = findGreatestIndex(seq, n);
      if (j !== -1)
        pre[i] = is[j];
      if (j === l2) {
        l2++;
        seq[l2] = n;
        is[l2] = i;
      } else if (n < seq[j + 1]) {
        seq[j + 1] = n;
        is[j + 1] = i;
      }
    }
    for (i = is[l2]; l2 >= 0; i = pre[i], l2--)
      seq[l2] = i;
    return seq;
  }
  function findGreatestIndex(seq, n) {
    let lo = -1, hi = seq.length;
    if (hi > 0 && seq[hi - 1] <= n)
      return hi - 1;
    while (hi - lo > 1) {
      let mid = Math.floor((lo + hi) / 2);
      if (seq[mid] > n)
        hi = mid;
      else
        lo = mid;
    }
    return lo;
  }
  F.prototype.updateNonKeyedChildrenNodes = function(parent, currentChildren, updatedChildren) {
    let currentChildrenLength = currentChildren.length, updatedChildrenLength = updatedChildren.length, commonLength = Math.min(currentChildrenLength, updatedChildrenLength);
    for (let i = 0; i < commonLength; ++i)
      updatedChildren[i] = this.updateAllNodes(parent, currentChildren[i], updatedChildren[i]);
    if (currentChildrenLength < updatedChildrenLength)
      for (let i = commonLength; i < updatedChildrenLength; ++i)
        updatedChildren[i] = this.checkCreateAllNodes(parent, updatedChildren[i]);
    else if (currentChildrenLength > updatedChildrenLength)
      for (let i = commonLength; i < currentChildrenLength; ++i)
        parent.removeChild(currentChildren[i].node);
  };
  F.prototype.updateNodeData = function(node, currentNodeData, updatedHtml, isSvg) {
    updateStyles(node, currentNodeData.styles, updatedHtml.nodeData.styles);
    updateAttributes(node, currentNodeData.attributes, updatedHtml.nodeData.attributes);
    updateClasses(node, currentNodeData.classes, updatedHtml.nodeData.classes, isSvg);
    updateProperties(node, currentNodeData.properties, updatedHtml.nodeData.properties);
    this.updateEvents(node, currentNodeData.events, updatedHtml);
  };
  function updateStyles(node, currentStyles, updatedStyles) {
    if (currentStyles === void 0) {
      if (updatedStyles !== void 0)
        createStyles(node, updatedStyles);
    } else if (updatedStyles === void 0) {
      if (currentStyles !== void 0)
        node.removeAttribute("style");
    } else {
      let matchCount = 0;
      for (let key2 in currentStyles) {
        let current = currentStyles[key2], updated = updatedStyles[key2], hasUpdated = updatedStyles[key2] !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node.style.setProperty(key2, updated);
          else
            node.style.removeProperty(key2);
      }
      let newKeys = Object.keys(updatedStyles), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key2 = newKeys[i];
        if (currentStyles[key2] === void 0) {
          let updated = updatedStyles[key2];
          ++matchCount;
          node.style.setProperty(key2, updated);
        }
      }
    }
  }
  function updateClasses(node, currentClasses, updatedClasses, isSvg) {
    let classUpdated = updatedClasses !== void 0 && updatedClasses.length > 0;
    if (currentClasses !== void 0 && currentClasses.length > 0 && !classUpdated)
      createClasses(node, [], isSvg);
    else if (classUpdated)
      createClasses(node, updatedClasses, isSvg);
  }
  function updateAttributes(node, currentAttributes, updatedAttributes) {
    if (currentAttributes === void 0) {
      if (updatedAttributes !== void 0)
        createAttributes(node, updatedAttributes);
    } else if (updatedAttributes === void 0) {
      if (currentAttributes !== void 0)
        for (let key2 in currentAttributes)
          node.removeAttribute(key2);
    } else {
      let matchCount = 0;
      for (let key2 in currentAttributes) {
        let current = currentAttributes[key2], updated = updatedAttributes[key2], hasUpdated = updated !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node.setAttribute(key2, updated);
          else
            node.removeAttribute(key2);
      }
      let newKeys = Object.keys(updatedAttributes), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key2 = newKeys[i];
        if (currentAttributes[key2] === void 0) {
          let updated = updatedAttributes[key2];
          ++matchCount;
          node.setAttribute(key2, updated);
        }
      }
    }
  }
  function updateProperties(node, currentProperties, updatedProperties) {
    let addAll = currentProperties === void 0, removeAll = updatedProperties === void 0;
    if (addAll) {
      if (!removeAll)
        for (let key2 in updatedProperties)
          node[key2] = updatedProperties[key2];
    } else if (removeAll) {
      if (!addAll)
        for (let key2 in currentProperties)
          node.removeAttribute(key2);
    } else {
      let matchCount = 0;
      for (let key2 in currentProperties) {
        let current = currentProperties[key2], updated = updatedProperties[key2], hasUpdated = updated !== void 0;
        if (hasUpdated)
          matchCount++;
        if (current !== updated)
          if (hasUpdated)
            node[key2] = updated;
          else
            node.removeAttribute(key2);
      }
      let newKeys = Object.keys(updatedProperties), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key2 = newKeys[i];
        if (currentProperties[key2] === void 0) {
          let updated = updatedProperties[key2];
          ++matchCount;
          node[key2] = updated;
        }
      }
    }
  }
  F.prototype.updateEvents = function(node, currentEvents, updatedHtml) {
    let updatedEvents = updatedHtml.nodeData.events;
    if (currentEvents === void 0) {
      if (updatedEvents !== void 0)
        this.createAllEvents(node, updatedHtml);
    } else if (updatedEvents === void 0) {
      if (currentEvents !== void 0)
        for (let key2 in currentEvents)
          this.removeEvent(node, key2);
    } else {
      let matchCount = 0;
      for (let key2 in currentEvents) {
        let current = currentEvents[key2], updated = updatedEvents[key2], hasUpdated = false;
        if (updated === void 0)
          this.removeEvent(node, key2);
        else {
          let currentLength = current.length, updatedLength = updated.length;
          if (currentLength != updatedLength)
            hasUpdated = true;
          else {
            for (let i = 0; i < currentLength; ++i)
              if (current[i] != updated[i]) {
                hasUpdated = true;
                break;
              }
          }
        }
        if (hasUpdated) {
          matchCount++;
          this.removeEvent(node, key2);
          this.createEvent(node, key2, updatedHtml);
        }
      }
      let newKeys = Object.keys(updatedEvents), newKeysLength = newKeys.length;
      for (let i = 0; matchCount < newKeysLength && i < newKeysLength; ++i) {
        let key2 = newKeys[i];
        if (currentEvents[key2] === void 0) {
          ++matchCount;
          this.createEvent(node, key2, updatedHtml);
        }
      }
    }
  };
  F.prototype.removeEvent = function(node, name2) {
    let eventKey = eventPrefix + name2;
    if (nonBubblingEvents.includes(name2)) {
      let runNonBubblingEvent = node[eventKey];
      node.removeEventListener(name2, runNonBubblingEvent, false);
    } else {
      let count = --this.applicationEvents.get(name2).count;
      if (count === 0) {
        this.root.removeEventListener(name2, this.applicationEvents.get(name2).handler, false);
        this.applicationEvents.delete(name2);
      }
    }
    node[eventKey + eventPostfix] = void 0;
    node[eventKey] = void 0;
  };
  function lazyMessageMap(mapper, html) {
    html.messageMapper = mapper;
    if (html.children !== void 0 && html.children.length > 0)
      for (let i = 0; i < html.children.length; ++i)
        lazyMessageMap(mapper, html.children[i]);
  }

  // output/Flame.Renderer.Internal.Dom/index.js
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var resume = /* @__PURE__ */ runEffectFn2(resume_);
  var maybeUpdater = function(updater) {
    return function(v) {
      if (v instanceof Just) {
        return updater(v.value0);
      }
      ;
      return pure2(unit);
    };
  };
  var start = function(parent) {
    return function(updater) {
      return runEffectFn4(start_)(Just.create)(parent)(maybeUpdater(updater));
    };
  };
  var startFrom = function(parent) {
    return function(updater) {
      return runEffectFn4(startFrom_)(Just.create)(parent)(maybeUpdater(updater));
    };
  };

  // output/Flame.Subscription.Internal.Listener/foreign.js
  var applicationIds = /* @__PURE__ */ new Set();
  function checkApplicationId_(id3) {
    if (applicationIds.has(id3))
      throw `Error mounting application: id ${id3} already registered!`;
    applicationIds.add(id3);
  }

  // output/Flame.Types/index.js
  var Window = /* @__PURE__ */ function() {
    function Window2() {
    }
    ;
    Window2.value = new Window2();
    return Window2;
  }();
  var Document = /* @__PURE__ */ function() {
    function Document2() {
    }
    ;
    Document2.value = new Document2();
    return Document2;
  }();
  var Custom = /* @__PURE__ */ function() {
    function Custom2() {
    }
    ;
    Custom2.value = new Custom2();
    return Custom2;
  }();

  // output/Foreign/foreign.js
  var isArray = Array.isArray || function(value) {
    return Object.prototype.toString.call(value) === "[object Array]";
  };

  // output/Foreign/index.js
  var unsafeFromForeign = unsafeCoerce2;

  // output/Flame.Subscription.Internal.Listener/index.js
  var createSubscription = function(updater) {
    return function(v) {
      if (v.value0 instanceof Window) {
        return createWindowListener(v.value1.value0)(function($13) {
          return updater(v.value1.value1.value0($13));
        });
      }
      ;
      if (v.value0 instanceof Document) {
        return createDocumentListener(v.value1.value0)(function($14) {
          return updater(v.value1.value1.value0($14));
        });
      }
      ;
      if (v.value0 instanceof Custom) {
        return createCustomListener(v.value1.value0)(function($15) {
          return updater(v.value1.value1.value0($15));
        });
      }
      ;
      throw new Error("Failed pattern match at Flame.Subscription.Internal.Listener (line 31, column 83 - line 34, column 76): " + [v.value0.constructor.name]);
    };
  };
  var checkApplicationId = /* @__PURE__ */ runEffectFn1(checkApplicationId_);
  var createMessageListener = function(appId) {
    return function(updater) {
      return function __do() {
        checkApplicationId(appId)();
        return createCustomListener(appId)(function($16) {
          return updater(unsafeFromForeign($16));
        })();
      };
    };
  };

  // output/Flame.Application.EffectList/index.js
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableArray);
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableArray);
  var map7 = /* @__PURE__ */ map(functorMaybe);
  var showId = function(dictShow) {
    var show9 = show(dictShow);
    return function(v) {
      return show9(v);
    };
  };
  var run3 = function(parent) {
    return function(isResumed) {
      return function(appId) {
        return function(v) {
          return function __do() {
            var modelState = $$new(v.init.value0)();
            var renderingState = $$new(21)();
            var render4 = function(model) {
              return function __do2() {
                var rendering2 = read(renderingState)();
                resume(rendering2)(v.view(model))();
                return write(model)(modelState)();
              };
            };
            var runUpdate = function(message2) {
              return function __do2() {
                var currentModel = read(modelState)();
                var v1 = v.update(currentModel)(message2);
                when2(modelHasChanged(currentModel)(v1.value0))(render4(v1.value0))();
                return runMessages(v1.value1)();
              };
            };
            var runMessages = function(affs) {
              return for_2(affs)(runAff_(function(v1) {
                if (v1 instanceof Left) {
                  return log(message(v1.value0));
                }
                ;
                if (v1 instanceof Right && v1.value0 instanceof Just) {
                  return runUpdate(v1.value0.value0);
                }
                ;
                return pure3(unit);
              }));
            };
            var rendering = function() {
              if (isResumed) {
                return startFrom(parent)(runUpdate)(v.view(v.init.value0))();
              }
              ;
              return start(parent)(runUpdate)(v.view(v.init.value0))();
            }();
            write(rendering)(renderingState)();
            runMessages(v.init.value1)();
            (function() {
              if (appId instanceof Nothing) {
                return unit;
              }
              ;
              if (appId instanceof Just) {
                return createMessageListener(appId.value0)(runUpdate)();
              }
              ;
              throw new Error("Failed pattern match at Flame.Application.EffectList (line 142, column 7 - line 144, column 63): " + [appId.constructor.name]);
            })();
            return traverse_2(createSubscription(runUpdate))(v.subscribe)();
          };
        };
      };
    };
  };
  var noAppId = /* @__PURE__ */ function() {
    return Nothing.value;
  }();
  var mountWith = function(dictShow) {
    var showId1 = showId(dictShow);
    return function(v) {
      return function(appId) {
        return function(application) {
          return function __do() {
            var maybeElement = querySelector(v)();
            if (maybeElement instanceof Just) {
              return run3(maybeElement.value0)(false)(map7(showId1)(appId))(application)();
            }
            ;
            if (maybeElement instanceof Nothing) {
              return $$throw("Error mounting application")();
            }
            ;
            throw new Error("Failed pattern match at Flame.Application.EffectList (line 102, column 7 - line 104, column 63): " + [maybeElement.constructor.name]);
          };
        };
      };
    };
  };
  var mountWith1 = /* @__PURE__ */ mountWith(showUnit);
  var mount_ = function(selector) {
    return mountWith1(selector)(noAppId);
  };

  // output/Flame.Application.NoEffects/index.js
  var mount_2 = function(selector) {
    return function(application) {
      return mount_(selector)({
        view: application.view,
        subscribe: application.subscribe,
        init: new Tuple(application.init, []),
        update: function(model) {
          return function(message2) {
            return new Tuple(application.update(model)(message2), []);
          };
        }
      });
    };
  };

  // output/Flame.Html.Event/foreign.js
  var messageEventData = 5;
  var rawEventData = 6;
  function createEvent_(name2) {
    return function(message2) {
      return [messageEventData, name2, message2];
    };
  }
  function createRawEvent_(name2) {
    return function(handler) {
      return [rawEventData, name2, handler];
    };
  }
  function nodeValue_(event) {
    if (event.target.contentEditable === true || event.target.contentEditable === "true" || event.target.contentEditable === "")
      return event.target.innerText;
    return event.target.value;
  }
  function key_(event) {
    if (event.type === "keyup" || event.type === "keydown" || event.type === "keypress")
      return event.key;
    return "";
  }

  // output/Flame.Html.Event/index.js
  var nodeValue = /* @__PURE__ */ runEffectFn1(nodeValue_);
  var key = /* @__PURE__ */ runEffectFn1(key_);
  var keyInput = function(constructor) {
    return function(event) {
      return function __do() {
        var down = key(event)();
        var value = nodeValue(event)();
        return Just.create(constructor(new Tuple(down, value)));
      };
    };
  };
  var createRawEvent = function(name2) {
    return function(handler) {
      return createRawEvent_(name2)(handler);
    };
  };
  var onKeyup = function(constructor) {
    return createRawEvent("keyup")(keyInput(constructor));
  };
  var createEvent = function(name2) {
    return function(message2) {
      return createEvent_(name2)(message2);
    };
  };
  var onClick = /* @__PURE__ */ createEvent("click");

  // output/Data.Show.Generic/foreign.js
  var intercalate4 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsNoArguments = {
    genericShowArgs: function(v) {
      return [];
    }
  };
  var genericShowArgsArgument = function(dictShow) {
    var show9 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show9(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowArgsProduct = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictGenericShowArgs1) {
      var genericShowArgs2 = genericShowArgs(dictGenericShowArgs1);
      return {
        genericShowArgs: function(v) {
          return append3(genericShowArgs1(v.value0))(genericShowArgs2(v.value1));
        }
      };
    };
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate4(" ")(append3([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShowSum = function(dictGenericShow) {
    var genericShow$prime1 = genericShow$prime(dictGenericShow);
    return function(dictGenericShow1) {
      var genericShow$prime2 = genericShow$prime(dictGenericShow1);
      return {
        "genericShow'": function(v) {
          if (v instanceof Inl) {
            return genericShow$prime1(v.value0);
          }
          ;
          if (v instanceof Inr) {
            return genericShow$prime2(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
        }
      };
    };
  };
  var genericShow = function(dictGeneric) {
    var from3 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x2) {
        return genericShow$prime1(from3(x2));
      };
    };
  };

  // output/Effect.Exception.Unsafe/index.js
  var unsafeThrowException = function($1) {
    return unsafePerformEffect(throwException($1));
  };
  var unsafeThrow = function($2) {
    return unsafeThrowException(error($2));
  };

  // output/LambdaCalc/index.js
  var genericShowArgsProduct2 = /* @__PURE__ */ genericShowArgsProduct(/* @__PURE__ */ genericShowArgsArgument(showInt));
  var genericShowArgsArgument2 = /* @__PURE__ */ genericShowArgsArgument(showString);
  var genericShowSum2 = /* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsArgument2)({
    reflectSymbol: function() {
      return "Con";
    }
  }));
  var AppIsSymbol = {
    reflectSymbol: function() {
      return "App";
    }
  };
  var LamIsSymbol = {
    reflectSymbol: function() {
      return "Lam";
    }
  };
  var PairIsSymbol = {
    reflectSymbol: function() {
      return "Pair";
    }
  };
  var FstIsSymbol = {
    reflectSymbol: function() {
      return "Fst";
    }
  };
  var SndIsSymbol = {
    reflectSymbol: function() {
      return "Snd";
    }
  };
  var SetIsSymbol = {
    reflectSymbol: function() {
      return "Set";
    }
  };
  var DomIsSymbol = {
    reflectSymbol: function() {
      return "Dom";
    }
  };
  var RngIsSymbol = {
    reflectSymbol: function() {
      return "Rng";
    }
  };
  var CctIsSymbol = {
    reflectSymbol: function() {
      return "Cct";
    }
  };
  var SplIsSymbol = {
    reflectSymbol: function() {
      return "Spl";
    }
  };
  var PushIsSymbol = {
    reflectSymbol: function() {
      return "Push";
    }
  };
  var ProjIsSymbol = {
    reflectSymbol: function() {
      return "Proj";
    }
  };
  var map8 = /* @__PURE__ */ map(functorList);
  var show3 = /* @__PURE__ */ show(showInt);
  var max3 = /* @__PURE__ */ max(ordInt);
  var append12 = /* @__PURE__ */ append(semigroupList);
  var pure4 = /* @__PURE__ */ pure(applicativeList);
  var $$null3 = /* @__PURE__ */ $$null(foldableArray);
  var bind4 = /* @__PURE__ */ bind(bindArray);
  var fix2 = /* @__PURE__ */ fix(lazyFn);
  var VC = /* @__PURE__ */ function() {
    function VC2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    VC2.create = function(value0) {
      return function(value1) {
        return new VC2(value0, value1);
      };
    };
    return VC2;
  }();
  var Con = /* @__PURE__ */ function() {
    function Con2(value0) {
      this.value0 = value0;
    }
    ;
    Con2.create = function(value0) {
      return new Con2(value0);
    };
    return Con2;
  }();
  var Var = /* @__PURE__ */ function() {
    function Var2(value0) {
      this.value0 = value0;
    }
    ;
    Var2.create = function(value0) {
      return new Var2(value0);
    };
    return Var2;
  }();
  var App2 = /* @__PURE__ */ function() {
    function App3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    App3.create = function(value0) {
      return function(value1) {
        return new App3(value0, value1);
      };
    };
    return App3;
  }();
  var Lam = /* @__PURE__ */ function() {
    function Lam2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Lam2.create = function(value0) {
      return function(value1) {
        return new Lam2(value0, value1);
      };
    };
    return Lam2;
  }();
  var Pair = /* @__PURE__ */ function() {
    function Pair2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Pair2.create = function(value0) {
      return function(value1) {
        return new Pair2(value0, value1);
      };
    };
    return Pair2;
  }();
  var Fst = /* @__PURE__ */ function() {
    function Fst2(value0) {
      this.value0 = value0;
    }
    ;
    Fst2.create = function(value0) {
      return new Fst2(value0);
    };
    return Fst2;
  }();
  var Snd = /* @__PURE__ */ function() {
    function Snd2(value0) {
      this.value0 = value0;
    }
    ;
    Snd2.create = function(value0) {
      return new Snd2(value0);
    };
    return Snd2;
  }();
  var $$Set = /* @__PURE__ */ function() {
    function $$Set2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    $$Set2.create = function(value0) {
      return function(value1) {
        return new $$Set2(value0, value1);
      };
    };
    return $$Set2;
  }();
  var Dom = /* @__PURE__ */ function() {
    function Dom2(value0) {
      this.value0 = value0;
    }
    ;
    Dom2.create = function(value0) {
      return new Dom2(value0);
    };
    return Dom2;
  }();
  var Rng = /* @__PURE__ */ function() {
    function Rng2(value0) {
      this.value0 = value0;
    }
    ;
    Rng2.create = function(value0) {
      return new Rng2(value0);
    };
    return Rng2;
  }();
  var Cct = /* @__PURE__ */ function() {
    function Cct2(value0) {
      this.value0 = value0;
    }
    ;
    Cct2.create = function(value0) {
      return new Cct2(value0);
    };
    return Cct2;
  }();
  var Spl = /* @__PURE__ */ function() {
    function Spl2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Spl2.create = function(value0) {
      return function(value1) {
        return new Spl2(value0, value1);
      };
    };
    return Spl2;
  }();
  var Push = /* @__PURE__ */ function() {
    function Push2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Push2.create = function(value0) {
      return function(value1) {
        return new Push2(value0, value1);
      };
    };
    return Push2;
  }();
  var Proj = /* @__PURE__ */ function() {
    function Proj2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Proj2.create = function(value0) {
      return function(value1) {
        return new Proj2(value0, value1);
      };
    };
    return Proj2;
  }();
  var genericVarName_ = {
    to: function(x1) {
      return new VC(x1.value0, x1.value1);
    },
    from: function(x1) {
      return new Product(x1.value0, x1.value1);
    }
  };
  var showVarName = {
    show: /* @__PURE__ */ genericShow(genericVarName_)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsProduct2(genericShowArgsArgument2))({
      reflectSymbol: function() {
        return "VC";
      }
    }))
  };
  var genericShowArgsArgument1 = /* @__PURE__ */ genericShowArgsArgument(showVarName);
  var genericShowSum1 = /* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsArgument1)({
    reflectSymbol: function() {
      return "Var";
    }
  }));
  var genericShowArgsProduct1 = /* @__PURE__ */ genericShowArgsProduct(genericShowArgsArgument1);
  var genericTerm_ = {
    to: function(x1) {
      if (x1 instanceof Inl) {
        return new Con(x1.value0);
      }
      ;
      if (x1 instanceof Inr && x1.value0 instanceof Inl) {
        return new Var(x1.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && x1.value0.value0 instanceof Inl)) {
        return new App2(x1.value0.value0.value0.value0, x1.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && x1.value0.value0.value0 instanceof Inl))) {
        return new Lam(x1.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0 instanceof Inl)))) {
        return new Pair(x1.value0.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0 instanceof Inl))))) {
        return new Fst(x1.value0.value0.value0.value0.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
        return new Snd(x1.value0.value0.value0.value0.value0.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))) {
        return new $$Set(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value0.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))) {
        return new Dom(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))) {
        return new Rng(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))) {
        return new Cct(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))) {
        return new Spl(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))) {
        return new Push(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value1);
      }
      ;
      if (x1 instanceof Inr && (x1.value0 instanceof Inr && (x1.value0.value0 instanceof Inr && (x1.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr)))))))))))) {
        return new Proj(x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0, x1.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value1);
      }
      ;
      throw new Error("Failed pattern match at LambdaCalc (line 54, column 1 - line 54, column 31): " + [x1.constructor.name]);
    },
    from: function(x1) {
      if (x1 instanceof Con) {
        return new Inl(x1.value0);
      }
      ;
      if (x1 instanceof Var) {
        return new Inr(new Inl(x1.value0));
      }
      ;
      if (x1 instanceof App2) {
        return new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1))));
      }
      ;
      if (x1 instanceof Lam) {
        return new Inr(new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1)))));
      }
      ;
      if (x1 instanceof Pair) {
        return new Inr(new Inr(new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1))))));
      }
      ;
      if (x1 instanceof Fst) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x1.value0))))));
      }
      ;
      if (x1 instanceof Snd) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x1.value0)))))));
      }
      ;
      if (x1 instanceof $$Set) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1)))))))));
      }
      ;
      if (x1 instanceof Dom) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x1.value0)))))))));
      }
      ;
      if (x1 instanceof Rng) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x1.value0))))))))));
      }
      ;
      if (x1 instanceof Cct) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(x1.value0)))))))))));
      }
      ;
      if (x1 instanceof Spl) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1)))))))))))));
      }
      ;
      if (x1 instanceof Push) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(new Product(x1.value0, x1.value1))))))))))))));
      }
      ;
      if (x1 instanceof Proj) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Product(x1.value0, x1.value1))))))))))))));
      }
      ;
      throw new Error("Failed pattern match at LambdaCalc (line 54, column 1 - line 54, column 31): " + [x1.constructor.name]);
    }
  };
  var genericShow2 = /* @__PURE__ */ genericShow(genericTerm_);
  var showTerm = {
    show: function(term) {
      return genericShow2(genericShowSum2(genericShowSum1(genericShowSum(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showTerm))(genericShowArgsArgument(showTerm)))(AppIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsProduct1(genericShowArgsArgument(showTerm)))(LamIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showTerm))(genericShowArgsArgument(showTerm)))(PairIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsArgument(showTerm))(FstIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsArgument(showTerm))(SndIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showTerm))(genericShowArgsArgument(showTerm)))(SetIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsArgument(showTerm))(DomIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsArgument(showTerm))(RngIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsArgument(showTerm))(CctIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsProduct2(genericShowArgsArgument(showTerm)))(SplIsSymbol))(genericShowSum(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showTerm))(genericShowArgsArgument(showTerm)))(PushIsSymbol))(genericShowConstructor(genericShowArgsProduct2(genericShowArgsArgument(showTerm)))(ProjIsSymbol)))))))))))))))(term);
    }
  };
  var show1 = /* @__PURE__ */ show(showTerm);
  var eqVarName = {
    eq: function(x1) {
      return function(y1) {
        return x1.value0 === y1.value0 && x1.value1 === y1.value1;
      };
    }
  };
  var eq2 = /* @__PURE__ */ eq(eqVarName);
  var elem3 = /* @__PURE__ */ elem(foldableArray)(eqVarName);
  var eqTerm = {
    eq: function(x1) {
      return function(y1) {
        if (x1 instanceof Con && y1 instanceof Con) {
          return x1.value0 === y1.value0;
        }
        ;
        if (x1 instanceof Var && y1 instanceof Var) {
          return eq2(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof App2 && y1 instanceof App2) {
          return eq(eqTerm)(x1.value0)(y1.value0) && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Lam && y1 instanceof Lam) {
          return eq2(x1.value0)(y1.value0) && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Pair && y1 instanceof Pair) {
          return eq(eqTerm)(x1.value0)(y1.value0) && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Fst && y1 instanceof Fst) {
          return eq(eqTerm)(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof Snd && y1 instanceof Snd) {
          return eq(eqTerm)(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof $$Set && y1 instanceof $$Set) {
          return eq(eqTerm)(x1.value0)(y1.value0) && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Dom && y1 instanceof Dom) {
          return eq(eqTerm)(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof Rng && y1 instanceof Rng) {
          return eq(eqTerm)(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof Cct && y1 instanceof Cct) {
          return eq(eqTerm)(x1.value0)(y1.value0);
        }
        ;
        if (x1 instanceof Spl && y1 instanceof Spl) {
          return x1.value0 === y1.value0 && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Push && y1 instanceof Push) {
          return eq(eqTerm)(x1.value0)(y1.value0) && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        if (x1 instanceof Proj && y1 instanceof Proj) {
          return x1.value0 === y1.value0 && eq(eqTerm)(x1.value1)(y1.value1);
        }
        ;
        return false;
      };
    }
  };
  var var_stock = /* @__PURE__ */ function() {
    return map8(VC.create(0))(new Cons("s", new Cons("t", new Cons("u", new Cons("v", new Cons("w", new Cons("a", new Cons("b", new Cons("c", Nil.value)))))))));
  }();
  var unwind = function($copy_v) {
    return function($copy_t) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_var_t = $copy_t;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, t, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return t;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = v;
            $tco_var_t = new App2(t, v(v1.value0));
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at LambdaCalc (line 137, column 1 - line 137, column 19): " + [v.constructor.name, t.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $tco_var_t, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var tuple = function(v) {
    if (v instanceof Nil) {
      return unsafeThrow("not enough vars");
    }
    ;
    if (v instanceof Cons && v.value1 instanceof Nil) {
      return v.value0;
    }
    ;
    if (v instanceof Cons) {
      return new Pair(v.value0, tuple(v.value1));
    }
    ;
    throw new Error("Failed pattern match at LambdaCalc (line 165, column 1 - line 165, column 42): " + [v.constructor.name]);
  };
  var showVar = function(v) {
    if (v.value0 === 0) {
      return v.value1;
    }
    ;
    if (otherwise) {
      return v.value1 + show3(v.value0);
    }
    ;
    throw new Error("Failed pattern match at LambdaCalc (line 45, column 1 - line 46, column 58): " + [v.constructor.name]);
  };
  var set$prime = /* @__PURE__ */ function() {
    return flip($$Set.create);
  }();
  var set = identity;
  var rewind = function(t) {
    return function(v) {
      if (v instanceof Nil) {
        return t;
      }
      ;
      if (v instanceof Cons) {
        return new Lam(v.value0, rewind(t)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at LambdaCalc (line 140, column 1 - line 140, column 17): " + [t.constructor.name, v.constructor.name]);
    };
  };
  var parens = function(s) {
    return "(" + (s + ")");
  };
  var showLeft = function(disp) {
    return function(v) {
      if (v instanceof Lam) {
        return parens(disp(v));
      }
      ;
      return disp(v);
    };
  };
  var showRight = function(disp) {
    return function(v) {
      if (v instanceof $$Set) {
        return disp(v);
      }
      ;
      if (v instanceof Pair) {
        return disp(v);
      }
      ;
      if (v instanceof Var) {
        return disp(v);
      }
      ;
      if (v instanceof Con) {
        return disp(v);
      }
      ;
      return parens(disp(v));
    };
  };
  var occurs = function(v) {
    return function(v1) {
      if (v instanceof Con) {
        return new Tuple(false, v1);
      }
      ;
      if (v instanceof Var) {
        if (!(v1.value1 === v.value0.value1)) {
          return new Tuple(false, v1);
        }
        ;
        if (v1.value0 === v.value0.value0) {
          return new Tuple(true, v1);
        }
        ;
        if (otherwise) {
          return new Tuple(false, v.value0);
        }
        ;
      }
      ;
      if (v instanceof App2) {
        var v3 = occurs(v.value0)(v1);
        var v4 = occurs(v.value1)(v1);
        return new Tuple(v3.value0 || v4.value0, function() {
          var $561 = v3.value1.value0 > v4.value1.value0;
          if ($561) {
            return v3.value1;
          }
          ;
          return v4.value1;
        }());
      }
      ;
      if (v instanceof Pair) {
        var v3 = occurs(v.value0)(v1);
        var v4 = occurs(v.value1)(v1);
        return new Tuple(v3.value0 || v4.value0, function() {
          var $574 = v3.value1.value0 > v4.value1.value0;
          if ($574) {
            return v3.value1;
          }
          ;
          return v4.value1;
        }());
      }
      ;
      if (v instanceof Fst) {
        return occurs(v.value0)(v1);
      }
      ;
      if (v instanceof Snd) {
        return occurs(v.value0)(v1);
      }
      ;
      if (v instanceof $$Set) {
        var v3 = occurs(v.value0)(v1);
        var v4 = occurs(v.value1)(v1);
        return new Tuple(v3.value0 || v4.value0, function() {
          var $589 = v3.value1.value0 > v4.value1.value0;
          if ($589) {
            return v3.value1;
          }
          ;
          return v4.value1;
        }());
      }
      ;
      if (v instanceof Dom) {
        return occurs(v.value0)(v1);
      }
      ;
      if (v instanceof Rng) {
        return occurs(v.value0)(v1);
      }
      ;
      if (v instanceof Cct) {
        return occurs(v.value0)(v1);
      }
      ;
      if (v instanceof Spl) {
        return occurs(v.value1)(v1);
      }
      ;
      if (v instanceof Push) {
        var v3 = occurs(v.value0)(v1);
        var v4 = occurs(v.value1)(v1);
        return new Tuple(v3.value0 || v4.value0, function() {
          var $607 = v3.value1.value0 > v4.value1.value0;
          if ($607) {
            return v3.value1;
          }
          ;
          return v4.value1;
        }());
      }
      ;
      if (v instanceof Proj) {
        return occurs(v.value1)(v1);
      }
      ;
      if (v instanceof Lam) {
        if (eq2(v.value0)(v1)) {
          return new Tuple(false, v1);
        }
        ;
        if (otherwise) {
          return occurs(v.value1)(v1);
        }
        ;
      }
      ;
      throw new Error("Failed pattern match at LambdaCalc (line 213, column 1 - line 213, column 33): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var make_var = /* @__PURE__ */ function() {
    var $1027 = VC.create(0);
    return function($1028) {
      return Var.create($1027($1028));
    };
  }();
  var p2 = /* @__PURE__ */ make_var("p");
  var x = /* @__PURE__ */ make_var("x");
  var make_con = /* @__PURE__ */ function() {
    return Con.create;
  }();
  var lam = function(v) {
    return function(v1) {
      if (v instanceof Var) {
        return new Lam(v.value0, v1);
      }
      ;
      return unsafeThrow("ill-formed abstraction");
    };
  };
  var make_set = function(p12) {
    return new $$Set(new App2(make_con("some"), p12), lam(x)(x));
  };
  var ix = /* @__PURE__ */ function() {
    return flip(Proj.create);
  }();
  var get_rng = /* @__PURE__ */ function() {
    return Rng.create;
  }();
  var get_dom = /* @__PURE__ */ function() {
    return Dom.create;
  }();
  var free_vars = function(term) {
    var go = function(v) {
      return function(bound) {
        return function(free) {
          if (v instanceof Con) {
            return free;
          }
          ;
          if (v instanceof Var) {
            var $638 = elem3(v.value0)(bound);
            if ($638) {
              return free;
            }
            ;
            return cons2(v.value0)(free);
          }
          ;
          if (v instanceof App2) {
            return go(v.value0)(bound)(go(v.value1)(bound)(free));
          }
          ;
          if (v instanceof Lam) {
            return go(v.value1)(cons2(v.value0)(bound))(free);
          }
          ;
          if (v instanceof Pair) {
            return go(v.value0)(bound)(go(v.value1)(bound)(free));
          }
          ;
          if (v instanceof Fst) {
            return go(v.value0)(bound)(free);
          }
          ;
          if (v instanceof Snd) {
            return go(v.value0)(bound)(free);
          }
          ;
          if (v instanceof $$Set) {
            return go(v.value0)(bound)(go(v.value1)(bound)(free));
          }
          ;
          if (v instanceof Dom) {
            return go(v.value0)(bound)(free);
          }
          ;
          if (v instanceof Rng) {
            return go(v.value0)(bound)(free);
          }
          ;
          if (v instanceof Cct) {
            return go(v.value0)(bound)(free);
          }
          ;
          if (v instanceof Spl) {
            return go(v.value1)(bound)(free);
          }
          ;
          if (v instanceof Push) {
            return go(v.value0)(bound)(go(v.value1)(bound)(free));
          }
          ;
          if (v instanceof Proj) {
            return go(v.value1)(bound)(free);
          }
          ;
          throw new Error("Failed pattern match at LambdaCalc (line 483, column 5 - line 483, column 33): " + [v.constructor.name, bound.constructor.name, free.constructor.name]);
        };
      };
    };
    return go(term)([])([]);
  };
  var enough_vars = function(t) {
    return function(vs) {
      var go = function(v) {
        return function(v1) {
          if (v1 instanceof Nil) {
            return unsafeThrow("exceeded variable stock");
          }
          ;
          if (v instanceof Pair && v1 instanceof Cons) {
            return new Cons(v1.value0, go(v.value1)(v1.value1));
          }
          ;
          if (v1 instanceof Cons) {
            return new Cons(v1.value0, Nil.value);
          }
          ;
          throw new Error("Failed pattern match at LambdaCalc (line 174, column 5 - line 174, column 53): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return go(t)(vs);
    };
  };
  var default_term_form = function(dictCategory) {
    return {
      "lam'": "\\",
      "arr'": ". ",
      "app'": " ",
      "con'": identity(dictCategory),
      "var'": showVar,
      "lb'": "[",
      "rb'": "]",
      "mid'": " | ",
      "la'": "<",
      "ra'": ">",
      "fst'": "fst ",
      "snd'": "snd ",
      "elem'": " <- ",
      "dom'": "sdom ",
      "rng'": "srng ",
      "prep'": ":"
    };
  };
  var default_term_form1 = /* @__PURE__ */ default_term_form(categoryFn);
  var conc = /* @__PURE__ */ function() {
    return Cct.create;
  }();
  var check_eta = function(v) {
    if (v instanceof Lam && (v.value1 instanceof App2 && (v.value1.value1 instanceof Var && (eq2(v.value0)(v.value1.value1.value0) && function() {
      var v2 = occurs(v.value1.value0)(v.value0);
      return !v2.value0;
    }())))) {
      return v.value1.value0;
    }
    ;
    return v;
  };
  var bump_color = function(v) {
    return function(v1) {
      return new VC(max3(v.value0)(v1.value0) + 1 | 0, v.value1);
    };
  };
  var bump_color$prime = function(v) {
    return function(v1) {
      var $685 = v.value1 === v1.value1;
      if ($685) {
        return bump_color(v)(v1);
      }
      ;
      return v;
    };
  };
  var subst = function(v) {
    return function(v1) {
      return function(v2) {
        if (v instanceof Con) {
          return v;
        }
        ;
        if (v2 instanceof Var && eq2(v1)(v2.value0)) {
          return v;
        }
        ;
        if (v instanceof Var) {
          if (eq2(v.value0)(v1)) {
            return v2;
          }
          ;
          if (otherwise) {
            return v;
          }
          ;
        }
        ;
        if (v instanceof Pair) {
          return new Pair(subst(v.value0)(v1)(v2), subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Fst) {
          return new Fst(subst(v.value0)(v1)(v2));
        }
        ;
        if (v instanceof Snd) {
          return new Snd(subst(v.value0)(v1)(v2));
        }
        ;
        if (v instanceof $$Set) {
          return new $$Set(subst(v.value0)(v1)(v2), subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Dom) {
          return new Dom(subst(v.value0)(v1)(v2));
        }
        ;
        if (v instanceof Rng) {
          return new Rng(subst(v.value0)(v1)(v2));
        }
        ;
        if (v instanceof App2) {
          return new App2(subst(v.value0)(v1)(v2), subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Cct) {
          return new Cct(subst(v.value0)(v1)(v2));
        }
        ;
        if (v instanceof Spl) {
          return new Spl(v.value0, subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Push) {
          return new Push(subst(v.value0)(v1)(v2), subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Proj) {
          return new Proj(v.value0, subst(v.value1)(v1)(v2));
        }
        ;
        if (v instanceof Lam && eq2(v1)(v.value0)) {
          return v;
        }
        ;
        if (v instanceof Lam) {
          var v4 = occurs(v2)(v.value0);
          var v5 = function() {
            if (v4.value0) {
              var x_uniq_st_v = bump_color$prime(bump_color(v.value0)(v4.value1))(v1);
              var v6 = occurs(v.value1)(x_uniq_st_v);
              var x_unique = function() {
                if (v6.value0) {
                  return bump_color(x_uniq_st_v)(v6.value1);
                }
                ;
                return x_uniq_st_v;
              }();
              return new Tuple(x_unique, subst(v.value1)(v.value0)(new Var(x_unique)));
            }
            ;
            return new Tuple(v.value0, v.value1);
          }();
          return new Lam(v5.value0, subst(v5.value1)(v1)(v2));
        }
        ;
        throw new Error("Failed pattern match at LambdaCalc (line 179, column 1 - line 179, column 24): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var unrollDom = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof $$Set) {
          var vars$prime = enough_vars(v1.value0)(v2);
          var occs = map8(function(s) {
            return new Tuple(occurs(v1)(s), s);
          })(vars$prime);
          var vars = map8(function(v3) {
            if (v3.value0.value0) {
              return bump_color(v3.value0.value1)(v3.value1);
            }
            ;
            return v3.value1;
          })(occs);
          var getvar = function(v3) {
            if (v3 instanceof Cons) {
              return new Tuple(new Var(v3.value0), v3.value1);
            }
            ;
            if (v3 instanceof Nil) {
              return unsafeThrow("could not getvar while unrolling dom");
            }
            ;
            throw new Error("Failed pattern match at LambdaCalc (line 148, column 7 - line 148, column 39): " + [v3.constructor.name]);
          };
          var go = function(t) {
            return function(vs1) {
              return function(apps) {
                var v3 = getvar(vs1);
                if (t instanceof Pair) {
                  return new Pair(v(unwind(v)(t.value0)(apps)), go(t.value1)(v3.value1)(new Cons(v3.value0, apps)));
                }
                ;
                return v(unwind(v)(t)(apps));
              };
            };
          };
          return new Tuple(go(v1.value0)(vars)(Nil.value), vars);
        }
        ;
        return unsafeThrow("trying to unroll dom: " + show1(v1));
      };
    };
  };
  var a = /* @__PURE__ */ make_var("a");
  var _2 = /* @__PURE__ */ function() {
    return Snd.create;
  }();
  var _1 = /* @__PURE__ */ function() {
    return Fst.create;
  }();
  var check_sigma = function(v) {
    if (v instanceof $$Set && (v.value0 instanceof Pair && (v.value0.value1 instanceof Lam && v.value0.value1.value1 instanceof $$Set))) {
      var abs2 = function(v2) {
        return function(v3) {
          if (v3 instanceof Pair) {
            return new Pair(new Lam(v2, v3.value0), abs2(v2)(v3.value1));
          }
          ;
          return new Lam(v2, v3);
        };
      };
      var rng = lam(p2)(new App2(v.value1, new Pair(_1(p2), new App2(new App2(new Lam(v.value0.value1.value0, v.value0.value1.value1.value1), _1(p2)), _2(new Spl(1, p2))))));
      var dom = new Pair(v.value0.value0, abs2(v.value0.value1.value0)(v.value0.value1.value1.value0));
      return new $$Set(dom, rng);
    }
    ;
    return v;
  };
  var show_term = function(term) {
    return show_formatted_term(default_term_form1)(term)(100);
  };
  var show_formatted_term = function(form) {
    return function(term) {
      return function(depth) {
        if (depth <= 0) {
          return "...";
        }
        ;
        if (otherwise) {
          var showt$prime = function(term2) {
            return show_formatted_term(form)(term2)(depth - 1 | 0);
          };
          var showt = function(v) {
            if (v instanceof Con) {
              return form["con'"](v.value0);
            }
            ;
            if (v instanceof Var) {
              return form["var'"](v.value0);
            }
            ;
            if (v instanceof Lam) {
              return form["lam'"] + (form["var'"](v.value0) + (form["arr'"] + showt$prime(v.value1)));
            }
            ;
            if (v instanceof App2) {
              return showLeft(showt$prime)(v.value0) + (form["app'"] + showRight(showt$prime)(v.value1));
            }
            ;
            if (v instanceof Pair) {
              return form["la'"] + (showt$prime(v.value0) + (", " + (showt$prime(v.value1) + form["ra'"])));
            }
            ;
            if (v instanceof Fst) {
              return form["fst'"] + showRight(showt)(v.value0);
            }
            ;
            if (v instanceof Snd) {
              return form["snd'"] + showRight(showt)(v.value0);
            }
            ;
            if (v instanceof $$Set) {
              var v1 = unrollDom($$eval)(v)(var_stock);
              var getvar = function(v2) {
                if (v2 instanceof Cons) {
                  return new Tuple(new Var(v2.value0), v2.value1);
                }
                ;
                if (v2 instanceof Nil) {
                  return unsafeThrow("getvar error in show_term");
                }
                ;
                throw new Error("Failed pattern match at LambdaCalc (line 434, column 13 - line 434, column 45): " + [v2.constructor.name]);
              };
              var showDom = function(t1) {
                return function(vs) {
                  var v2 = getvar(vs);
                  if (t1 instanceof Pair) {
                    return showt(v2.value0) + (form["elem'"] + (showt(t1.value0) + (", " + showDom(t1.value1)(v2.value1))));
                  }
                  ;
                  return showt(v2.value0) + (form["elem'"] + showt(t1));
                };
              };
              return form["lb'"] + (showt$prime($$eval(new App2(v.value1, tuple(map8(Var.create)(v1.value1))))) + (form["mid'"] + (showDom(v1.value0)(v1.value1) + form["rb'"])));
            }
            ;
            if (v instanceof Dom) {
              return form["dom'"] + showRight(showt)(v.value0);
            }
            ;
            if (v instanceof Rng) {
              return form["rng'"] + showRight(showt)(v.value0);
            }
            ;
            if (v instanceof Cct) {
              return "concat " + showRight(showt)(v.value0);
            }
            ;
            if (v instanceof Spl) {
              return "splitAt " + (show3(v.value0) + (" " + showRight(showt)(v.value1)));
            }
            ;
            if (v instanceof Push) {
              return showt(v.value0) + (form["prep'"] + showt(v.value1));
            }
            ;
            if (v instanceof Proj) {
              return showt(v.value1) + ("~{" + (show3(v.value0) + "}"));
            }
            ;
            throw new Error("Failed pattern match at LambdaCalc (line 424, column 13 - line 448, column 51): " + [v.constructor.name]);
          };
          return showt(term);
        }
        ;
        throw new Error("Failed pattern match at LambdaCalc (line 420, column 1 - line 449, column 60): " + [form.constructor.name, term.constructor.name, depth.constructor.name]);
      };
    };
  };
  var rerollDom = function(d) {
    return function(d$prime) {
      return function(vs) {
        var linearize = function(v) {
          return function(d1) {
            if (v instanceof Pair) {
              return new Pair(v.value0, linearize(v.value1)(d1));
            }
            ;
            return new Pair(v, d1);
          };
        };
        var go = function(v) {
          return function(v1) {
            return function(v2) {
              if (v instanceof Pair && v2 instanceof Cons) {
                return new Pair(rewind(v.value0)(v1), go(v.value1)(append12(v1)(pure4(v2.value0)))(v2.value1));
              }
              ;
              if (v instanceof Pair && v2 instanceof Nil) {
                return unsafeThrow("not enough vars for: " + show_term(v));
              }
              ;
              return rewind(v)(v1);
            };
          };
        };
        return go(linearize(d)(d$prime))(Nil.value)(vs);
      };
    };
  };
  var openEval = function(eval$prime) {
    return function(e) {
      return function(s) {
        var rightSplit = function($copy_n) {
          return function($copy_v) {
            var $tco_var_n = $copy_n;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(n, v4) {
              if (v4 instanceof Pair) {
                var $812 = n === 1;
                if ($812) {
                  $tco_done = true;
                  return v4.value1;
                }
                ;
                $tco_var_n = n - 1 | 0;
                $copy_v = v4.value1;
                return;
              }
              ;
              $tco_done = true;
              return unsafeThrow("bad rightSplit " + (show3(n) + (": " + (show_term(p2) + (" within " + show_term(e))))));
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_n, $copy_v);
            }
            ;
            return $tco_result;
          };
        };
        var leftSplit = function(n) {
          return function(v4) {
            if (v4 instanceof Pair) {
              var $816 = n === 1;
              if ($816) {
                return v4.value0;
              }
              ;
              return new Pair(v4.value0, leftSplit(n - 1 | 0)(v4.value1));
            }
            ;
            return unsafeThrow("bad leftSplit " + (show3(n) + (": " + (show_term(p2) + (" within " + show_term(e))))));
          };
        };
        var ev = function(t) {
          return eval$prime(t)(Nil.value);
        };
        if (e instanceof Con && s instanceof Nil) {
          return e;
        }
        ;
        if (e instanceof Con) {
          return unwind(ev)(e)(s);
        }
        ;
        if (e instanceof Var && s instanceof Nil) {
          return e;
        }
        ;
        if (e instanceof Var) {
          return unwind(ev)(e)(s);
        }
        ;
        if (e instanceof Lam && s instanceof Nil) {
          return check_eta(new Lam(e.value0, ev(e.value1)));
        }
        ;
        if (e instanceof Lam && s instanceof Cons) {
          return eval$prime(subst(e.value1)(e.value0)(s.value0))(s.value1);
        }
        ;
        if (e instanceof App2) {
          return eval$prime(e.value0)(new Cons(e.value1, s));
        }
        ;
        if (e instanceof Pair && s instanceof Nil) {
          return new Pair(ev(e.value0), ev(e.value1));
        }
        ;
        if (e instanceof Pair && s instanceof Cons) {
          return unsafeThrow("trying to apply a pair: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        if (e instanceof Fst) {
          var v = ev(e.value0);
          if (v instanceof Pair) {
            return eval$prime(v.value0)(s);
          }
          ;
          return unwind(ev)(new Fst(v))(s);
        }
        ;
        if (e instanceof Snd) {
          var v = ev(e.value0);
          if (v instanceof Pair) {
            return eval$prime(v.value1)(s);
          }
          ;
          return unwind(ev)(new Snd(v))(s);
        }
        ;
        if (e instanceof $$Set && s instanceof Nil) {
          return new $$Set(ev(e.value0), ev(e.value1));
        }
        ;
        if (e instanceof $$Set && s instanceof Cons) {
          return unsafeThrow("trying to apply a set: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        if (e instanceof Dom && s instanceof Nil) {
          var v = ev(e.value0);
          if (v instanceof $$Set) {
            return ev(v.value0);
          }
          ;
          return new Dom(v);
        }
        ;
        if (e instanceof Dom && s instanceof Cons) {
          return unsafeThrow("trying to apply a dom: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        if (e instanceof Rng) {
          var v = ev(e.value0);
          if (v instanceof $$Set) {
            return eval$prime(v.value1)(s);
          }
          ;
          return unwind(ev)(new Rng(v))(s);
        }
        ;
        if (e instanceof Cct && s instanceof Nil) {
          var v = ev(e.value0);
          if (v instanceof $$Set && (v.value1 instanceof Lam && (v.value1.value1 instanceof $$Set && $$null3(bind4([v.value0, v.value1.value1.value0])(free_vars))))) {
            var v2 = unrollDom(ev)(v)(var_stock);
            var v3 = unrollDom(ev)(ev(new App2(v.value1, tuple(map8(Var.create)(v2.value1)))))(drop(length2(v2.value1))(var_stock));
            var dom = rerollDom(v2.value0)(v3.value0)(append12(v2.value1)(v3.value1));
            return check_sigma(ev(new $$Set(dom, lam(p2)(new App2(get_rng(new App2(v.value1, _1(new Spl(length2(v2.value1), p2)))), _2(new Spl(length2(v2.value1), p2)))))));
          }
          ;
          return new Cct(v);
        }
        ;
        if (e instanceof Cct && s instanceof Cons) {
          return unsafeThrow("trying to apply a set: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        if (e instanceof Spl && s instanceof Nil) {
          var v = ev(e.value1);
          if (v instanceof Pair) {
            return new Pair(leftSplit(e.value0)(v), rightSplit(e.value0)(v));
          }
          ;
          return new Spl(e.value0, v);
        }
        ;
        if (e instanceof Spl) {
          return unsafeThrow("trying to split something weird: " + (show_term(e) + (" to " + show_term(a))));
        }
        ;
        if (e instanceof Push && s instanceof Nil) {
          return new Push(ev(e.value0), ev(e.value1));
        }
        ;
        if (e instanceof Push && s instanceof Cons) {
          return unsafeThrow("trying to apply a push: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        if (e instanceof Proj && s instanceof Nil) {
          var v = ev(e.value1);
          if (v instanceof Push) {
            var $895 = e.value0 === 0;
            if ($895) {
              return eval$prime(v.value0)(s);
            }
            ;
            return eval$prime(new Proj(e.value0 - 1 | 0, v.value1))(s);
          }
          ;
          return new Proj(e.value0, v);
        }
        ;
        if (e instanceof Proj && s instanceof Cons) {
          return unsafeThrow("trying to apply a proj: " + (show_term(e) + (" to " + show_term(s.value0))));
        }
        ;
        throw new Error("Failed pattern match at LambdaCalc (line 57, column 22 - line 107, column 81): " + [e.constructor.name, s.constructor.name]);
      };
    };
  };
  var $$eval = function(term) {
    return fix2(openEval)(term)(Nil.value);
  };
  var openFinal = function(eval$prime) {
    return function(e) {
      return function(s) {
        var ev = function(t) {
          return openFinal(eval$prime)(t)(Nil.value);
        };
        if (e instanceof Dom && s instanceof Nil) {
          var v = ev(e.value0);
          if (v instanceof $$Set) {
            return ev(v.value0);
          }
          ;
          return v;
        }
        ;
        if (e instanceof Rng) {
          var v = ev(e.value0);
          if (v instanceof $$Set) {
            return eval$prime(v.value1)(s);
          }
          ;
          return eval$prime(lam(a)(a))(s);
        }
        ;
        if (e instanceof Cct && s instanceof Nil) {
          var v = ev(e.value0);
          if (v instanceof $$Set && (v.value1 instanceof Lam && v.value1.value1 instanceof $$Set)) {
            var v2 = unrollDom(ev)(v)(var_stock);
            var v3 = unrollDom(ev)(ev(new App2(v.value1, tuple(map8(Var.create)(v2.value1)))))(drop(length2(v2.value1))(var_stock));
            var dom = rerollDom(v2.value0)(v3.value0)(append12(v2.value1)(v3.value1));
            return check_sigma(ev(new $$Set(dom, lam(p2)(new App2(get_rng(new App2(v.value1, _1(new Spl(length2(v2.value1), p2)))), _2(new Spl(length2(v2.value1), p2)))))));
          }
          ;
          if (v instanceof $$Set && v.value1 instanceof Lam) {
            return check_sigma(openFinal(eval$prime)(new Cct(new $$Set(v.value0, new Lam(v.value1.value0, new $$Set(v.value1.value1, lam(a)(a))))))(Nil.value));
          }
          ;
          return new Cct(v);
        }
        ;
        return eval$prime(e)(s);
      };
    };
  };
  var evalFinal = function(term) {
    return fix2(function($1029) {
      return openFinal(openEval($1029));
    })(term)(Nil.value);
  };

  // output/Data.Bounded.Generic/index.js
  var genericTopNoArguments = /* @__PURE__ */ function() {
    return {
      "genericTop'": NoArguments.value
    };
  }();
  var genericTop$prime = function(dict) {
    return dict["genericTop'"];
  };
  var genericTopConstructor = function(dictGenericTop) {
    return {
      "genericTop'": genericTop$prime(dictGenericTop)
    };
  };
  var genericTopSum = function(dictGenericTop) {
    return {
      "genericTop'": new Inr(genericTop$prime(dictGenericTop))
    };
  };
  var genericTop = function(dictGeneric) {
    var to3 = to(dictGeneric);
    return function(dictGenericTop) {
      return to3(genericTop$prime(dictGenericTop));
    };
  };
  var genericBottomNoArguments = /* @__PURE__ */ function() {
    return {
      "genericBottom'": NoArguments.value
    };
  }();
  var genericBottom$prime = function(dict) {
    return dict["genericBottom'"];
  };
  var genericBottomConstructor = function(dictGenericBottom) {
    return {
      "genericBottom'": genericBottom$prime(dictGenericBottom)
    };
  };
  var genericBottomSum = function(dictGenericBottom) {
    return {
      "genericBottom'": new Inl(genericBottom$prime(dictGenericBottom))
    };
  };
  var genericBottom = function(dictGeneric) {
    var to3 = to(dictGeneric);
    return function(dictGenericBottom) {
      return to3(genericBottom$prime(dictGenericBottom));
    };
  };

  // output/Data.Enum.Generic/index.js
  var map9 = /* @__PURE__ */ map(functorMaybe);
  var genericSucc$prime = function(dict) {
    return dict["genericSucc'"];
  };
  var genericSucc = function(dictGeneric) {
    var to3 = to(dictGeneric);
    var from3 = from(dictGeneric);
    return function(dictGenericEnum) {
      var $156 = map9(to3);
      var $157 = genericSucc$prime(dictGenericEnum);
      return function($158) {
        return $156($157(from3($158)));
      };
    };
  };
  var genericPred$prime = function(dict) {
    return dict["genericPred'"];
  };
  var genericPred = function(dictGeneric) {
    var to3 = to(dictGeneric);
    var from3 = from(dictGeneric);
    return function(dictGenericEnum) {
      var $159 = map9(to3);
      var $160 = genericPred$prime(dictGenericEnum);
      return function($161) {
        return $159($160(from3($161)));
      };
    };
  };
  var genericEnumSum = function(dictGenericEnum) {
    var genericPred$prime1 = genericPred$prime(dictGenericEnum);
    var genericSucc$prime1 = genericSucc$prime(dictGenericEnum);
    return function(dictGenericTop) {
      var genericTop$prime2 = genericTop$prime(dictGenericTop);
      return function(dictGenericEnum1) {
        var genericPred$prime2 = genericPred$prime(dictGenericEnum1);
        var genericSucc$prime2 = genericSucc$prime(dictGenericEnum1);
        return function(dictGenericBottom) {
          var genericBottom$prime2 = genericBottom$prime(dictGenericBottom);
          return {
            "genericPred'": function(v) {
              if (v instanceof Inl) {
                return map9(Inl.create)(genericPred$prime1(v.value0));
              }
              ;
              if (v instanceof Inr) {
                var v1 = genericPred$prime2(v.value0);
                if (v1 instanceof Nothing) {
                  return new Just(new Inl(genericTop$prime2));
                }
                ;
                if (v1 instanceof Just) {
                  return new Just(new Inr(v1.value0));
                }
                ;
                throw new Error("Failed pattern match at Data.Enum.Generic (line 30, column 14 - line 32, column 31): " + [v1.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Enum.Generic (line 28, column 18 - line 32, column 31): " + [v.constructor.name]);
            },
            "genericSucc'": function(v) {
              if (v instanceof Inl) {
                var v1 = genericSucc$prime1(v.value0);
                if (v1 instanceof Nothing) {
                  return new Just(new Inr(genericBottom$prime2));
                }
                ;
                if (v1 instanceof Just) {
                  return new Just(new Inl(v1.value0));
                }
                ;
                throw new Error("Failed pattern match at Data.Enum.Generic (line 34, column 14 - line 36, column 31): " + [v1.constructor.name]);
              }
              ;
              if (v instanceof Inr) {
                return map9(Inr.create)(genericSucc$prime2(v.value0));
              }
              ;
              throw new Error("Failed pattern match at Data.Enum.Generic (line 33, column 18 - line 37, column 36): " + [v.constructor.name]);
            }
          };
        };
      };
    };
  };
  var genericEnumNoArguments = {
    "genericPred'": function(v) {
      return Nothing.value;
    },
    "genericSucc'": function(v) {
      return Nothing.value;
    }
  };
  var genericEnumConstructor = function(dictGenericEnum) {
    var genericPred$prime1 = genericPred$prime(dictGenericEnum);
    var genericSucc$prime1 = genericSucc$prime(dictGenericEnum);
    return {
      "genericPred'": function(v) {
        return map9(Constructor)(genericPred$prime1(v));
      },
      "genericSucc'": function(v) {
        return map9(Constructor)(genericSucc$prime1(v));
      }
    };
  };

  // output/Data.String.Utils/foreign.js
  function wordsImpl(s) {
    return s.split(/[\u000a-\u000d\u0085\u2028\u2029\u0009\u0020\u00a0\u1680\u2000-\u200a\u202f\u205f\u3000]+/);
  }

  // output/Data.String.Utils/index.js
  var words = function(s) {
    return wordsImpl(s);
  };

  // output/Control.Monad.State/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var mapState = function(f) {
    return mapStateT(function($19) {
      return Identity(f(unwrap3($19)));
    });
  };
  var evalState = function(v) {
    return function(s) {
      var v1 = v(s);
      return v1.value0;
    };
  };

  // output/Memo/index.js
  var bindStateT2 = /* @__PURE__ */ bindStateT(monadIdentity);
  var bind5 = /* @__PURE__ */ bind(bindStateT2);
  var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadIdentity);
  var gets2 = /* @__PURE__ */ gets(monadStateStateT2);
  var pure5 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadIdentity));
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var fix3 = /* @__PURE__ */ fix(lazyFn);
  var memoizeTag = function(dictOrd) {
    var ordTuple3 = ordTuple(dictOrd);
    return function(dictOrd1) {
      var ordTuple1 = ordTuple3(dictOrd1);
      var insert5 = insert2(ordTuple1);
      var lookup5 = lookup2(ordTuple1);
      return function(i) {
        return function(f) {
          return function(p5) {
            var extend2 = function(g2) {
              return function(w) {
                return new Tuple(fst(w), g2(w));
              };
            };
            var addkey = mapState(extend2(uncurry(insert5(new Tuple(i, p5)))))(f(p5));
            return bind5(gets2(lookup5(new Tuple(i, p5))))(maybe(addkey)(pure5));
          };
        };
      };
    };
  };
  var memoize = function(dictBind) {
    var bind13 = bind(dictBind);
    var discard1 = discard2(dictBind);
    return function(dictMonadState) {
      var gets1 = gets(dictMonadState);
      var pure111 = pure(dictMonadState.Monad0().Applicative0());
      var modify_2 = modify_(dictMonadState);
      return function(dictOrd) {
        var ordTuple3 = ordTuple(dictOrd);
        return function(dictOrd1) {
          var ordTuple1 = ordTuple3(dictOrd1);
          var lookup5 = lookup2(ordTuple1);
          var insert5 = insert2(ordTuple1);
          return function(f) {
            return function(v) {
              return bind13(gets1(lookup5(new Tuple(v.value0, v.value1.value0))))(function(v1) {
                if (v1 instanceof Just) {
                  return pure111(v1.value0);
                }
                ;
                return bind13(f(v))(function(y) {
                  return discard1(modify_2(insert5(new Tuple(v.value0, v.value1.value0))(y)))(function() {
                    return pure111(y);
                  });
                });
              });
            };
          };
        };
      };
    };
  };
  var memoize1 = /* @__PURE__ */ memoize(bindStateT2)(monadStateStateT2);
  var execute = function(m2) {
    return evalState(m2)(empty3);
  };
  var memo = function(dictOrd) {
    var memoize3 = memoize1(dictOrd);
    return function(dictOrd1) {
      var memoize4 = memoize3(dictOrd1);
      return function(f) {
        return function(x2) {
          return execute(fix3(function($67) {
            return memoize4(f($67));
          })(x2));
        };
      };
    };
  };

  // output/Utils/index.js
  var flippedApply = function(dictApply) {
    var lift24 = lift2(dictApply);
    return function(dictApply1) {
      return lift24(flip(apply(dictApply1)));
    };
  };
  var apmplus = function(dictApply) {
    var lift24 = lift2(dictApply);
    return function(dictSemigroup) {
      return lift24(append(dictSemigroup));
    };
  };

  // output/TDParseCFG/index.js
  var genericShowConstructor2 = /* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments);
  var genericEnumConstructor2 = /* @__PURE__ */ genericEnumConstructor(genericEnumNoArguments);
  var genericTopConstructor2 = /* @__PURE__ */ genericTopConstructor(genericTopNoArguments);
  var genericEnumSum2 = /* @__PURE__ */ genericEnumSum(genericEnumConstructor2)(genericTopConstructor2);
  var genericBottomConstructor2 = /* @__PURE__ */ genericBottomConstructor(genericBottomNoArguments);
  var genericBottomSum2 = /* @__PURE__ */ genericBottomSum(genericBottomConstructor2);
  var genericEnumSum1 = /* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(/* @__PURE__ */ genericEnumSum2(genericEnumConstructor2)(genericBottomConstructor2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2))(genericBottomSum2);
  var map10 = /* @__PURE__ */ map(functorList);
  var bind6 = /* @__PURE__ */ bind(bindList);
  var pure6 = /* @__PURE__ */ pure(applicativeList);
  var traverse2 = /* @__PURE__ */ traverse(traversableList);
  var bind1 = /* @__PURE__ */ bind(bindArray);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var bind22 = /* @__PURE__ */ bind(bindMaybe);
  var traverse12 = /* @__PURE__ */ traverse2(applicativeMaybe);
  var map1 = /* @__PURE__ */ map(functorMaybe);
  var lookup4 = /* @__PURE__ */ lookup(foldableList)(eqString);
  var pure1 = /* @__PURE__ */ pure(applicativeMaybe);
  var memo2 = /* @__PURE__ */ memo(ordInt)(ordInt);
  var any3 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var map22 = /* @__PURE__ */ map(functorArray);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var lift22 = /* @__PURE__ */ lift2(applyArray);
  var lift21 = /* @__PURE__ */ lift2(applyList);
  var set2 = /* @__PURE__ */ set(categoryFn);
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorList);
  var applySecond2 = /* @__PURE__ */ applySecond(applyList);
  var guard3 = /* @__PURE__ */ guard(alternativeList);
  var fix4 = /* @__PURE__ */ fix(lazyFn);
  var functorStateT2 = /* @__PURE__ */ functorStateT(functorIdentity);
  var map32 = /* @__PURE__ */ map(functorStateT2);
  var applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadIdentity);
  var bind32 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadIdentity));
  var sequence2 = /* @__PURE__ */ sequence(traversableList)(applicativeStateT2);
  var pure22 = /* @__PURE__ */ pure(applicativeStateT2);
  var S = /* @__PURE__ */ function() {
    function S2() {
    }
    ;
    S2.value = new S2();
    return S2;
  }();
  var R = /* @__PURE__ */ function() {
    function R2(value0) {
      this.value0 = value0;
    }
    ;
    R2.create = function(value0) {
      return new R2(value0);
    };
    return R2;
  }();
  var W = /* @__PURE__ */ function() {
    function W2(value0) {
      this.value0 = value0;
    }
    ;
    W2.create = function(value0) {
      return new W2(value0);
    };
    return W2;
  }();
  var C = /* @__PURE__ */ function() {
    function C2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    C2.create = function(value0) {
      return function(value1) {
        return new C2(value0, value1);
      };
    };
    return C2;
  }();
  var D = /* @__PURE__ */ function() {
    function D2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    D2.create = function(value0) {
      return function(value1) {
        return new D2(value0, value1);
      };
    };
    return D2;
  }();
  var U = /* @__PURE__ */ function() {
    function U2() {
    }
    ;
    U2.value = new U2();
    return U2;
  }();
  var E = /* @__PURE__ */ function() {
    function E2() {
    }
    ;
    E2.value = new E2();
    return E2;
  }();
  var T = /* @__PURE__ */ function() {
    function T2() {
    }
    ;
    T2.value = new T2();
    return T2;
  }();
  var G = /* @__PURE__ */ function() {
    function G2() {
    }
    ;
    G2.value = new G2();
    return G2;
  }();
  var Arr = /* @__PURE__ */ function() {
    function Arr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Arr2.create = function(value0) {
      return function(value1) {
        return new Arr2(value0, value1);
      };
    };
    return Arr2;
  }();
  var Eff = /* @__PURE__ */ function() {
    function Eff2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Eff2.create = function(value0) {
      return function(value1) {
        return new Eff2(value0, value1);
      };
    };
    return Eff2;
  }();
  var FA = /* @__PURE__ */ function() {
    function FA2() {
    }
    ;
    FA2.value = new FA2();
    return FA2;
  }();
  var BA = /* @__PURE__ */ function() {
    function BA2() {
    }
    ;
    BA2.value = new BA2();
    return BA2;
  }();
  var PM = /* @__PURE__ */ function() {
    function PM2() {
    }
    ;
    PM2.value = new PM2();
    return PM2;
  }();
  var FC = /* @__PURE__ */ function() {
    function FC2() {
    }
    ;
    FC2.value = new FC2();
    return FC2;
  }();
  var MR = /* @__PURE__ */ function() {
    function MR2(value0) {
      this.value0 = value0;
    }
    ;
    MR2.create = function(value0) {
      return new MR2(value0);
    };
    return MR2;
  }();
  var ML = /* @__PURE__ */ function() {
    function ML2(value0) {
      this.value0 = value0;
    }
    ;
    ML2.create = function(value0) {
      return new ML2(value0);
    };
    return ML2;
  }();
  var UR = /* @__PURE__ */ function() {
    function UR2(value0) {
      this.value0 = value0;
    }
    ;
    UR2.create = function(value0) {
      return new UR2(value0);
    };
    return UR2;
  }();
  var UL = /* @__PURE__ */ function() {
    function UL2(value0) {
      this.value0 = value0;
    }
    ;
    UL2.create = function(value0) {
      return new UL2(value0);
    };
    return UL2;
  }();
  var Z = /* @__PURE__ */ function() {
    function Z2() {
    }
    ;
    Z2.value = new Z2();
    return Z2;
  }();
  var A = /* @__PURE__ */ function() {
    function A2(value0) {
      this.value0 = value0;
    }
    ;
    A2.create = function(value0) {
      return new A2(value0);
    };
    return A2;
  }();
  var J = /* @__PURE__ */ function() {
    function J2(value0) {
      this.value0 = value0;
    }
    ;
    J2.create = function(value0) {
      return new J2(value0);
    };
    return J2;
  }();
  var Eps = /* @__PURE__ */ function() {
    function Eps2() {
    }
    ;
    Eps2.value = new Eps2();
    return Eps2;
  }();
  var DN = /* @__PURE__ */ function() {
    function DN2() {
    }
    ;
    DN2.value = new DN2();
    return DN2;
  }();
  var XL = /* @__PURE__ */ function() {
    function XL2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    XL2.create = function(value0) {
      return function(value1) {
        return new XL2(value0, value1);
      };
    };
    return XL2;
  }();
  var Lex = /* @__PURE__ */ function() {
    function Lex2(value0) {
      this.value0 = value0;
    }
    ;
    Lex2.create = function(value0) {
      return new Lex2(value0);
    };
    return Lex2;
  }();
  var Comb = /* @__PURE__ */ function() {
    function Comb2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Comb2.create = function(value0) {
      return function(value1) {
        return new Comb2(value0, value1);
      };
    };
    return Comb2;
  }();
  var Proof = /* @__PURE__ */ function() {
    function Proof2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Proof2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Proof2(value0, value1, value2, value3);
          };
        };
      };
    };
    return Proof2;
  }();
  var Leaf2 = /* @__PURE__ */ function() {
    function Leaf3(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Leaf3.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Leaf3(value0, value1, value2);
        };
      };
    };
    return Leaf3;
  }();
  var Branch = /* @__PURE__ */ function() {
    function Branch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Branch2.create = function(value0) {
      return function(value1) {
        return new Branch2(value0, value1);
      };
    };
    return Branch2;
  }();
  var Island = /* @__PURE__ */ function() {
    function Island2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Island2.create = function(value0) {
      return function(value1) {
        return new Island2(value0, value1);
      };
    };
    return Island2;
  }();
  var CP = /* @__PURE__ */ function() {
    function CP2() {
    }
    ;
    CP2.value = new CP2();
    return CP2;
  }();
  var Cmp = /* @__PURE__ */ function() {
    function Cmp2() {
    }
    ;
    Cmp2.value = new Cmp2();
    return Cmp2;
  }();
  var CBar = /* @__PURE__ */ function() {
    function CBar2() {
    }
    ;
    CBar2.value = new CBar2();
    return CBar2;
  }();
  var DBar = /* @__PURE__ */ function() {
    function DBar2() {
    }
    ;
    DBar2.value = new DBar2();
    return DBar2;
  }();
  var Cor = /* @__PURE__ */ function() {
    function Cor2() {
    }
    ;
    Cor2.value = new Cor2();
    return Cor2;
  }();
  var DP = /* @__PURE__ */ function() {
    function DP2() {
    }
    ;
    DP2.value = new DP2();
    return DP2;
  }();
  var Det = /* @__PURE__ */ function() {
    function Det2() {
    }
    ;
    Det2.value = new Det2();
    return Det2;
  }();
  var Gen = /* @__PURE__ */ function() {
    function Gen2() {
    }
    ;
    Gen2.value = new Gen2();
    return Gen2;
  }();
  var GenD = /* @__PURE__ */ function() {
    function GenD2() {
    }
    ;
    GenD2.value = new GenD2();
    return GenD2;
  }();
  var Dmp = /* @__PURE__ */ function() {
    function Dmp2() {
    }
    ;
    Dmp2.value = new Dmp2();
    return Dmp2;
  }();
  var NP = /* @__PURE__ */ function() {
    function NP2() {
    }
    ;
    NP2.value = new NP2();
    return NP2;
  }();
  var FN = /* @__PURE__ */ function() {
    function FN2() {
    }
    ;
    FN2.value = new FN2();
    return FN2;
  }();
  var RN = /* @__PURE__ */ function() {
    function RN2() {
    }
    ;
    RN2.value = new RN2();
    return RN2;
  }();
  var VP = /* @__PURE__ */ function() {
    function VP2() {
    }
    ;
    VP2.value = new VP2();
    return VP2;
  }();
  var TV = /* @__PURE__ */ function() {
    function TV2() {
    }
    ;
    TV2.value = new TV2();
    return TV2;
  }();
  var DV = /* @__PURE__ */ function() {
    function DV2() {
    }
    ;
    DV2.value = new DV2();
    return DV2;
  }();
  var AV = /* @__PURE__ */ function() {
    function AV2() {
    }
    ;
    AV2.value = new AV2();
    return AV2;
  }();
  var AdjP = /* @__PURE__ */ function() {
    function AdjP2() {
    }
    ;
    AdjP2.value = new AdjP2();
    return AdjP2;
  }();
  var TAdj = /* @__PURE__ */ function() {
    function TAdj2() {
    }
    ;
    TAdj2.value = new TAdj2();
    return TAdj2;
  }();
  var Deg = /* @__PURE__ */ function() {
    function Deg2() {
    }
    ;
    Deg2.value = new Deg2();
    return Deg2;
  }();
  var AdvP = /* @__PURE__ */ function() {
    function AdvP2() {
    }
    ;
    AdvP2.value = new AdvP2();
    return AdvP2;
  }();
  var TAdv = /* @__PURE__ */ function() {
    function TAdv2() {
    }
    ;
    TAdv2.value = new TAdv2();
    return TAdv2;
  }();
  var AdcP = /* @__PURE__ */ function() {
    function AdcP2() {
    }
    ;
    AdcP2.value = new AdcP2();
    return AdcP2;
  }();
  var Adc = /* @__PURE__ */ function() {
    function Adc2() {
    }
    ;
    Adc2.value = new Adc2();
    return Adc2;
  }();
  var showOp = {
    show: function(v) {
      if (v instanceof FA) {
        return ">";
      }
      ;
      if (v instanceof BA) {
        return "<";
      }
      ;
      if (v instanceof PM) {
        return "&";
      }
      ;
      if (v instanceof FC) {
        return ".";
      }
      ;
      if (v instanceof MR) {
        return "R";
      }
      ;
      if (v instanceof ML) {
        return "L";
      }
      ;
      if (v instanceof UL) {
        return "\xD9";
      }
      ;
      if (v instanceof UR) {
        return "\xDA";
      }
      ;
      if (v instanceof Z) {
        return "Z";
      }
      ;
      if (v instanceof A) {
        return "A";
      }
      ;
      if (v instanceof J) {
        return "J";
      }
      ;
      if (v instanceof Eps) {
        return "E";
      }
      ;
      if (v instanceof DN) {
        return "D";
      }
      ;
      if (v instanceof XL) {
        return "XL " + show(showOp)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at TDParseCFG (line 199, column 10 - line 213, column 30): " + [v.constructor.name]);
    }
  };
  var genericCat_ = {
    to: function(x2) {
      if (x2 instanceof Inl) {
        return CP.value;
      }
      ;
      if (x2 instanceof Inr && x2.value0 instanceof Inl) {
        return Cmp.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && x2.value0.value0 instanceof Inl)) {
        return CBar.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && x2.value0.value0.value0 instanceof Inl))) {
        return DBar.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0 instanceof Inl)))) {
        return Cor.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0 instanceof Inl))))) {
        return DP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
        return Det.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))) {
        return Gen.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))) {
        return GenD.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))) {
        return Dmp.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))) {
        return NP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))) {
        return FN.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))) {
        return RN.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))) {
        return VP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))) {
        return TV.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))))) {
        return DV.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))))) {
        return AV.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))))))) {
        return AdjP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))))))) {
        return TAdj.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))))))))) {
        return Deg.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))))))))) {
        return AdvP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))))))))))) {
        return TAdv.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))))))))))) {
        return AdcP.value;
      }
      ;
      if (x2 instanceof Inr && (x2.value0 instanceof Inr && (x2.value0.value0 instanceof Inr && (x2.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x2.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr)))))))))))))))))))))) {
        return Adc.value;
      }
      ;
      throw new Error("Failed pattern match at TDParseCFG (line 40, column 1 - line 40, column 30): " + [x2.constructor.name]);
    },
    from: function(x2) {
      if (x2 instanceof CP) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x2 instanceof Cmp) {
        return new Inr(new Inl(NoArguments.value));
      }
      ;
      if (x2 instanceof CBar) {
        return new Inr(new Inr(new Inl(NoArguments.value)));
      }
      ;
      if (x2 instanceof DBar) {
        return new Inr(new Inr(new Inr(new Inl(NoArguments.value))));
      }
      ;
      if (x2 instanceof Cor) {
        return new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))));
      }
      ;
      if (x2 instanceof DP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))));
      }
      ;
      if (x2 instanceof Det) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))));
      }
      ;
      if (x2 instanceof Gen) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))));
      }
      ;
      if (x2 instanceof GenD) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))));
      }
      ;
      if (x2 instanceof Dmp) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))));
      }
      ;
      if (x2 instanceof NP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))));
      }
      ;
      if (x2 instanceof FN) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))));
      }
      ;
      if (x2 instanceof RN) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))));
      }
      ;
      if (x2 instanceof VP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))));
      }
      ;
      if (x2 instanceof TV) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))));
      }
      ;
      if (x2 instanceof DV) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))))));
      }
      ;
      if (x2 instanceof AV) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))))));
      }
      ;
      if (x2 instanceof AdjP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))))))));
      }
      ;
      if (x2 instanceof TAdj) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))))))));
      }
      ;
      if (x2 instanceof Deg) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))))))))));
      }
      ;
      if (x2 instanceof AdvP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))))))))));
      }
      ;
      if (x2 instanceof TAdv) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))))))))))));
      }
      ;
      if (x2 instanceof AdcP) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))))))))))));
      }
      ;
      if (x2 instanceof Adc) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value)))))))))))))))))))))));
      }
      ;
      throw new Error("Failed pattern match at TDParseCFG (line 40, column 1 - line 40, column 30): " + [x2.constructor.name]);
    }
  };
  var showCat = {
    show: /* @__PURE__ */ genericShow(genericCat_)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "CP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Cmp";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "CBar";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "DBar";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Cor";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "DP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Det";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Gen";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "GenD";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Dmp";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "NP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "FN";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RN";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "VP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "TV";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "DV";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "AV";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "AdjP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "TAdj";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Deg";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "AdvP";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "TAdv";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "AdcP";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Adc";
      }
    })))))))))))))))))))))))))
  };
  var eqTy = {
    eq: function(x2) {
      return function(y) {
        if (x2 instanceof E && y instanceof E) {
          return true;
        }
        ;
        if (x2 instanceof T && y instanceof T) {
          return true;
        }
        ;
        if (x2 instanceof G && y instanceof G) {
          return true;
        }
        ;
        if (x2 instanceof Arr && y instanceof Arr) {
          return eq(eqTy)(x2.value0)(y.value0) && eq(eqTy)(x2.value1)(y.value1);
        }
        ;
        if (x2 instanceof Eff && y instanceof Eff) {
          return eq(eqF)(x2.value0)(y.value0) && eq(eqTy)(x2.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var eqF = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof U) {
          return true;
        }
        ;
        if (v1 instanceof U) {
          return true;
        }
        ;
        if (v instanceof S && v1 instanceof S) {
          return true;
        }
        ;
        if (v instanceof R && v1 instanceof R) {
          return eq(eqTy)(v.value0)(v1.value0);
        }
        ;
        if (v instanceof W && v1 instanceof W) {
          return eq(eqTy)(v.value0)(v1.value0);
        }
        ;
        if (v instanceof C && v1 instanceof C) {
          return eq(eqTy)(v.value0)(v1.value0) && eq(eqTy)(v.value1)(v1.value1);
        }
        ;
        if (v instanceof D && v1 instanceof D) {
          return eq(eqTy)(v.value0)(v1.value0) && eq(eqTy)(v.value1)(v1.value1);
        }
        ;
        return false;
      };
    }
  };
  var eq22 = /* @__PURE__ */ eq(eqF);
  var eq3 = /* @__PURE__ */ eq(eqTy);
  var eqOp = {
    eq: function(x2) {
      return function(y) {
        if (x2 instanceof FA && y instanceof FA) {
          return true;
        }
        ;
        if (x2 instanceof BA && y instanceof BA) {
          return true;
        }
        ;
        if (x2 instanceof PM && y instanceof PM) {
          return true;
        }
        ;
        if (x2 instanceof FC && y instanceof FC) {
          return true;
        }
        ;
        if (x2 instanceof MR && y instanceof MR) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof ML && y instanceof ML) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof UR && y instanceof UR) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof UL && y instanceof UL) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof Z && y instanceof Z) {
          return true;
        }
        ;
        if (x2 instanceof A && y instanceof A) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof J && y instanceof J) {
          return eq22(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof Eps && y instanceof Eps) {
          return true;
        }
        ;
        if (x2 instanceof DN && y instanceof DN) {
          return true;
        }
        ;
        if (x2 instanceof XL && y instanceof XL) {
          return eq22(x2.value0)(y.value0) && eq(eqOp)(x2.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var eq5 = /* @__PURE__ */ eq(eqOp);
  var ordTy = {
    compare: function(x2) {
      return function(y) {
        if (x2 instanceof E && y instanceof E) {
          return EQ.value;
        }
        ;
        if (x2 instanceof E) {
          return LT.value;
        }
        ;
        if (y instanceof E) {
          return GT.value;
        }
        ;
        if (x2 instanceof T && y instanceof T) {
          return EQ.value;
        }
        ;
        if (x2 instanceof T) {
          return LT.value;
        }
        ;
        if (y instanceof T) {
          return GT.value;
        }
        ;
        if (x2 instanceof G && y instanceof G) {
          return EQ.value;
        }
        ;
        if (x2 instanceof G) {
          return LT.value;
        }
        ;
        if (y instanceof G) {
          return GT.value;
        }
        ;
        if (x2 instanceof Arr && y instanceof Arr) {
          var v = compare(ordTy)(x2.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordTy)(x2.value1)(y.value1);
        }
        ;
        if (x2 instanceof Arr) {
          return LT.value;
        }
        ;
        if (y instanceof Arr) {
          return GT.value;
        }
        ;
        if (x2 instanceof Eff && y instanceof Eff) {
          var v = compare(ordF)(x2.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordTy)(x2.value1)(y.value1);
        }
        ;
        throw new Error("Failed pattern match at TDParseCFG (line 0, column 0 - line 0, column 0): " + [x2.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqTy;
    }
  };
  var ordF = {
    compare: function(x2) {
      return function(y) {
        if (x2 instanceof S && y instanceof S) {
          return EQ.value;
        }
        ;
        if (x2 instanceof S) {
          return LT.value;
        }
        ;
        if (y instanceof S) {
          return GT.value;
        }
        ;
        if (x2 instanceof R && y instanceof R) {
          return compare(ordTy)(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof R) {
          return LT.value;
        }
        ;
        if (y instanceof R) {
          return GT.value;
        }
        ;
        if (x2 instanceof W && y instanceof W) {
          return compare(ordTy)(x2.value0)(y.value0);
        }
        ;
        if (x2 instanceof W) {
          return LT.value;
        }
        ;
        if (y instanceof W) {
          return GT.value;
        }
        ;
        if (x2 instanceof C && y instanceof C) {
          var v = compare(ordTy)(x2.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordTy)(x2.value1)(y.value1);
        }
        ;
        if (x2 instanceof C) {
          return LT.value;
        }
        ;
        if (y instanceof C) {
          return GT.value;
        }
        ;
        if (x2 instanceof D && y instanceof D) {
          var v = compare(ordTy)(x2.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare(ordTy)(x2.value1)(y.value1);
        }
        ;
        if (x2 instanceof D) {
          return LT.value;
        }
        ;
        if (y instanceof D) {
          return GT.value;
        }
        ;
        if (x2 instanceof U && y instanceof U) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at TDParseCFG (line 0, column 0 - line 0, column 0): " + [x2.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqF;
    }
  };
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordTy)(ordTy);
  var eqCat = {
    eq: function(x2) {
      return function(y) {
        if (x2 instanceof CP && y instanceof CP) {
          return true;
        }
        ;
        if (x2 instanceof Cmp && y instanceof Cmp) {
          return true;
        }
        ;
        if (x2 instanceof CBar && y instanceof CBar) {
          return true;
        }
        ;
        if (x2 instanceof DBar && y instanceof DBar) {
          return true;
        }
        ;
        if (x2 instanceof Cor && y instanceof Cor) {
          return true;
        }
        ;
        if (x2 instanceof DP && y instanceof DP) {
          return true;
        }
        ;
        if (x2 instanceof Det && y instanceof Det) {
          return true;
        }
        ;
        if (x2 instanceof Gen && y instanceof Gen) {
          return true;
        }
        ;
        if (x2 instanceof GenD && y instanceof GenD) {
          return true;
        }
        ;
        if (x2 instanceof Dmp && y instanceof Dmp) {
          return true;
        }
        ;
        if (x2 instanceof NP && y instanceof NP) {
          return true;
        }
        ;
        if (x2 instanceof FN && y instanceof FN) {
          return true;
        }
        ;
        if (x2 instanceof RN && y instanceof RN) {
          return true;
        }
        ;
        if (x2 instanceof VP && y instanceof VP) {
          return true;
        }
        ;
        if (x2 instanceof TV && y instanceof TV) {
          return true;
        }
        ;
        if (x2 instanceof DV && y instanceof DV) {
          return true;
        }
        ;
        if (x2 instanceof AV && y instanceof AV) {
          return true;
        }
        ;
        if (x2 instanceof AdjP && y instanceof AdjP) {
          return true;
        }
        ;
        if (x2 instanceof TAdj && y instanceof TAdj) {
          return true;
        }
        ;
        if (x2 instanceof Deg && y instanceof Deg) {
          return true;
        }
        ;
        if (x2 instanceof AdvP && y instanceof AdvP) {
          return true;
        }
        ;
        if (x2 instanceof TAdv && y instanceof TAdv) {
          return true;
        }
        ;
        if (x2 instanceof AdcP && y instanceof AdcP) {
          return true;
        }
        ;
        if (x2 instanceof Adc && y instanceof Adc) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordCat = {
    compare: function(x2) {
      return function(y) {
        if (x2 instanceof CP && y instanceof CP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof CP) {
          return LT.value;
        }
        ;
        if (y instanceof CP) {
          return GT.value;
        }
        ;
        if (x2 instanceof Cmp && y instanceof Cmp) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Cmp) {
          return LT.value;
        }
        ;
        if (y instanceof Cmp) {
          return GT.value;
        }
        ;
        if (x2 instanceof CBar && y instanceof CBar) {
          return EQ.value;
        }
        ;
        if (x2 instanceof CBar) {
          return LT.value;
        }
        ;
        if (y instanceof CBar) {
          return GT.value;
        }
        ;
        if (x2 instanceof DBar && y instanceof DBar) {
          return EQ.value;
        }
        ;
        if (x2 instanceof DBar) {
          return LT.value;
        }
        ;
        if (y instanceof DBar) {
          return GT.value;
        }
        ;
        if (x2 instanceof Cor && y instanceof Cor) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Cor) {
          return LT.value;
        }
        ;
        if (y instanceof Cor) {
          return GT.value;
        }
        ;
        if (x2 instanceof DP && y instanceof DP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof DP) {
          return LT.value;
        }
        ;
        if (y instanceof DP) {
          return GT.value;
        }
        ;
        if (x2 instanceof Det && y instanceof Det) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Det) {
          return LT.value;
        }
        ;
        if (y instanceof Det) {
          return GT.value;
        }
        ;
        if (x2 instanceof Gen && y instanceof Gen) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Gen) {
          return LT.value;
        }
        ;
        if (y instanceof Gen) {
          return GT.value;
        }
        ;
        if (x2 instanceof GenD && y instanceof GenD) {
          return EQ.value;
        }
        ;
        if (x2 instanceof GenD) {
          return LT.value;
        }
        ;
        if (y instanceof GenD) {
          return GT.value;
        }
        ;
        if (x2 instanceof Dmp && y instanceof Dmp) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Dmp) {
          return LT.value;
        }
        ;
        if (y instanceof Dmp) {
          return GT.value;
        }
        ;
        if (x2 instanceof NP && y instanceof NP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof NP) {
          return LT.value;
        }
        ;
        if (y instanceof NP) {
          return GT.value;
        }
        ;
        if (x2 instanceof FN && y instanceof FN) {
          return EQ.value;
        }
        ;
        if (x2 instanceof FN) {
          return LT.value;
        }
        ;
        if (y instanceof FN) {
          return GT.value;
        }
        ;
        if (x2 instanceof RN && y instanceof RN) {
          return EQ.value;
        }
        ;
        if (x2 instanceof RN) {
          return LT.value;
        }
        ;
        if (y instanceof RN) {
          return GT.value;
        }
        ;
        if (x2 instanceof VP && y instanceof VP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof VP) {
          return LT.value;
        }
        ;
        if (y instanceof VP) {
          return GT.value;
        }
        ;
        if (x2 instanceof TV && y instanceof TV) {
          return EQ.value;
        }
        ;
        if (x2 instanceof TV) {
          return LT.value;
        }
        ;
        if (y instanceof TV) {
          return GT.value;
        }
        ;
        if (x2 instanceof DV && y instanceof DV) {
          return EQ.value;
        }
        ;
        if (x2 instanceof DV) {
          return LT.value;
        }
        ;
        if (y instanceof DV) {
          return GT.value;
        }
        ;
        if (x2 instanceof AV && y instanceof AV) {
          return EQ.value;
        }
        ;
        if (x2 instanceof AV) {
          return LT.value;
        }
        ;
        if (y instanceof AV) {
          return GT.value;
        }
        ;
        if (x2 instanceof AdjP && y instanceof AdjP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof AdjP) {
          return LT.value;
        }
        ;
        if (y instanceof AdjP) {
          return GT.value;
        }
        ;
        if (x2 instanceof TAdj && y instanceof TAdj) {
          return EQ.value;
        }
        ;
        if (x2 instanceof TAdj) {
          return LT.value;
        }
        ;
        if (y instanceof TAdj) {
          return GT.value;
        }
        ;
        if (x2 instanceof Deg && y instanceof Deg) {
          return EQ.value;
        }
        ;
        if (x2 instanceof Deg) {
          return LT.value;
        }
        ;
        if (y instanceof Deg) {
          return GT.value;
        }
        ;
        if (x2 instanceof AdvP && y instanceof AdvP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof AdvP) {
          return LT.value;
        }
        ;
        if (y instanceof AdvP) {
          return GT.value;
        }
        ;
        if (x2 instanceof TAdv && y instanceof TAdv) {
          return EQ.value;
        }
        ;
        if (x2 instanceof TAdv) {
          return LT.value;
        }
        ;
        if (y instanceof TAdv) {
          return GT.value;
        }
        ;
        if (x2 instanceof AdcP && y instanceof AdcP) {
          return EQ.value;
        }
        ;
        if (x2 instanceof AdcP) {
          return LT.value;
        }
        ;
        if (y instanceof AdcP) {
          return GT.value;
        }
        ;
        if (x2 instanceof Adc && y instanceof Adc) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at TDParseCFG (line 0, column 0 - line 0, column 0): " + [x2.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqCat;
    }
  };
  var enumCat = {
    succ: /* @__PURE__ */ genericSucc(genericCat_)(genericEnumSum1),
    pred: /* @__PURE__ */ genericPred(genericCat_)(genericEnumSum1),
    Ord0: function() {
      return ordCat;
    }
  };
  var commuteTy = {
    commutative: function(ty) {
      return eq3(ty)(T.value);
    }
  };
  var boundedCat = {
    top: /* @__PURE__ */ genericTop(genericCat_)(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(/* @__PURE__ */ genericTopSum(genericTopConstructor2)))))))))))))))))))))))),
    bottom: /* @__PURE__ */ genericBottom(genericCat_)(genericBottomSum2),
    Ord0: function() {
      return ordCat;
    }
  };
  var showNoIndices = function(v) {
    if (v instanceof S) {
      return "S";
    }
    ;
    if (v instanceof R) {
      return "R";
    }
    ;
    if (v instanceof W) {
      return "W";
    }
    ;
    if (v instanceof C) {
      return "C";
    }
    ;
    if (v instanceof D) {
      return "D";
    }
    ;
    if (v instanceof U) {
      return "_";
    }
    ;
    throw new Error("Failed pattern match at TDParseCFG (line 81, column 17 - line 87, column 15): " + [v.constructor.name]);
  };
  var semTerm = function(v) {
    if (v instanceof Lex) {
      return v.value0;
    }
    ;
    if (v instanceof Comb) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at TDParseCFG (line 454, column 1 - line 454, column 23): " + [v.constructor.name]);
  };
  var r = /* @__PURE__ */ make_var("r");
  var protoParse = function(dictMonad) {
    var Applicative0 = dictMonad.Applicative0();
    var pure32 = pure(Applicative0);
    var Bind1 = dictMonad.Bind1();
    var bind42 = bind(Bind1);
    var map42 = map(Bind1.Apply0().Functor0());
    var traverse22 = traverse2(Applicative0);
    return function(v) {
      return function(v1) {
        return function(v2) {
          if (v2.value1.value1 instanceof Cons && v2.value1.value1.value1 instanceof Nil) {
            return pure32(map10(function(v3) {
              return new Tuple(v3.value1.value0, new Leaf2(v2.value1.value1.value0.value0, v3.value0, v3.value1.value1));
            })(v2.value1.value1.value0.value1));
          }
          ;
          var mkIsland = function(v3) {
            if (v3 instanceof CP) {
              return Island.create;
            }
            ;
            return Branch.create;
          };
          var help = function(v3) {
            return bind42(v1(v3.value0))(function(parsesL) {
              return bind42(v1(v3.value1))(function(parsesR) {
                return pure32(bind6(parsesL)(function(v4) {
                  return bind6(parsesR)(function(v5) {
                    return bind6(v(v4.value0)(v5.value0))(function(cat) {
                      return pure6(new Tuple(cat, mkIsland(cat)(v4.value1)(v5.value1)));
                    });
                  });
                }));
              });
            });
          };
          var bisect = function(v3) {
            return bind6(range3(1)(length2(v3.value1.value1) - 1 | 0))(function(i) {
              var v4 = new Tuple(take2(i)(v3.value1.value1), drop(i)(v3.value1.value1));
              return pure6(new Tuple(new Tuple(v3.value0, new Tuple((v3.value0 + i | 0) - 1 | 0, v4.value0)), new Tuple(v3.value0 + i | 0, new Tuple(v3.value1.value0, v4.value1))));
            });
          };
          return map42(concat2)(traverse22(help)(bisect(v2)));
        };
      };
    };
  };
  var protoParse1 = /* @__PURE__ */ protoParse(/* @__PURE__ */ monadStateT(monadIdentity));
  var parse = function(cfg) {
    return function(dict) {
      return function(input3) {
        var clitics = ["'s"];
        var stripClitics = function(s) {
          return bind1(clitics)(function(c1) {
            return maybe([s])(function(w) {
              return [w, c1];
            })(stripSuffix(c1)(s));
          });
        };
        var lexes = fromFoldable7(bind1(words(input3))(stripClitics));
        return bind22(traverse12(function(s) {
          return map1(function(v) {
            return new Tuple(s, v);
          })(lookup4(s)(dict));
        })(lexes))(function(ws) {
          return pure1(map10(snd)(memo2(protoParse1(cfg))(new Tuple(0, new Tuple(length2(ws) - 1 | 0, ws)))));
        });
      };
    };
  };
  var p3 = /* @__PURE__ */ make_var("p");
  var op = /* @__PURE__ */ make_var("op");
  var mzeroTerm = function(v) {
    if (v instanceof T) {
      return make_con("true");
    }
    ;
    return make_con("this really shouldn't happen");
  };
  var mplusTerm = function(v) {
    if (v instanceof T) {
      return function(p12) {
        return function(q) {
          return new App2(new App2(make_con("and"), p12), q);
        };
      };
    }
    ;
    return function(v1) {
      return function(v2) {
        return make_con("this really shouldn't happen");
      };
    };
  };
  var monoid = function(v) {
    if (v instanceof T) {
      return true;
    }
    ;
    return false;
  };
  var mm = /* @__PURE__ */ make_var("mm");
  var m = /* @__PURE__ */ make_var("m");
  var l$prime = /* @__PURE__ */ make_var("l'");
  var l = /* @__PURE__ */ make_var("l");
  var k = /* @__PURE__ */ make_var("k");
  var hasType = function(t) {
    return function(v) {
      return eq3(t)(v.value2);
    };
  };
  var g = /* @__PURE__ */ make_var("g");
  var functor = function(v) {
    return true;
  };
  var extendTerm = function(v) {
    if (v instanceof W) {
      return lam(k)(lam(m)(new Pair(_1(m), new App2(k, m))));
    }
    ;
    return make_con("co-tastrophe");
  };
  var evaluated = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof E) {
        $tco_done = true;
        return true;
      }
      ;
      if (v instanceof T) {
        $tco_done = true;
        return true;
      }
      ;
      if (v instanceof G) {
        $tco_done = true;
        return true;
      }
      ;
      if (v instanceof Arr) {
        $copy_v = v.value1;
        return;
      }
      ;
      if (v instanceof Eff && v.value0 instanceof C) {
        $tco_done = true;
        return false;
      }
      ;
      if (v instanceof Eff) {
        $copy_v = v.value1;
        return;
      }
      ;
      throw new Error("Failed pattern match at TDParseCFG (line 104, column 13 - line 110, column 31): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var effW = function(w) {
    return Eff.create(new W(w));
  };
  var effS = /* @__PURE__ */ function() {
    return Eff.create(S.value);
  }();
  var effR = function(r1) {
    return Eff.create(new R(r1));
  };
  var effD = function(i) {
    return function(j) {
      return Eff.create(new D(i, j));
    };
  };
  var effC = function(r1) {
    return function(o) {
      return Eff.create(new C(r1, o));
    };
  };
  var counitTerm = /* @__PURE__ */ function() {
    return lam(m)(new App2(_2(m), _1(m)));
  }();
  var commutative = function(dict) {
    return dict.commutative;
  };
  var commutative1 = /* @__PURE__ */ commutative(commuteTy);
  var commuteF = {
    commutative: function(v) {
      if (v instanceof S) {
        return true;
      }
      ;
      if (v instanceof R) {
        return true;
      }
      ;
      if (v instanceof W) {
        return commutative1(v.value0);
      }
      ;
      if (v instanceof C) {
        return false;
      }
      ;
      if (v instanceof D) {
        return false;
      }
      ;
      if (v instanceof U) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at TDParseCFG (line 238, column 17 - line 244, column 19): " + [v.constructor.name]);
    }
  };
  var commutative2 = /* @__PURE__ */ commutative(commuteF);
  var norm = function(op1) {
    var startsWith = function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return false;
        }
        ;
        if (v1 instanceof Nil) {
          return true;
        }
        ;
        if (v instanceof Cons && v1 instanceof Cons) {
          return eq5(v.value0)(v1.value0) && startsWith(v.value1)(v1.value1);
        }
        ;
        throw new Error("Failed pattern match at TDParseCFG (line 425, column 5 - line 425, column 37): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    var anyOf = function(p12) {
      return function(as) {
        return any3(p12)(map22(fromFoldable7)(as));
      };
    };
    return function(v) {
      if (v instanceof UR) {
        return !anyOf(function(v1) {
          return startsWith(op1)(v1);
        })([[new MR(v.value0)], [DN.value, new MR(v.value0)]]);
      }
      ;
      if (v instanceof UL) {
        return !anyOf(function(v1) {
          return startsWith(op1)(v1);
        })([[new ML(v.value0)], [DN.value, new ML(v.value0)]]);
      }
      ;
      if (v instanceof DN) {
        return !anyOf(function(v1) {
          return startsWith(op1)(v1);
        })(append13(map22(function(m1) {
          return [m1(U.value), DN.value, m1(U.value)];
        })([MR.create, ML.create]))([[new ML(U.value), DN.value, new MR(U.value)], [new A(U.value), DN.value, new MR(U.value)], [new ML(U.value), DN.value, new A(U.value)], [Eps.value]]));
      }
      ;
      if (v instanceof J) {
        return !anyOf(function(v1) {
          return startsWith(op1)(v1);
        })(append13(lift22(function(k1) {
          return function(m1) {
            return append13([m1(v.value0)])(append13(k1)([m1(v.value0)]));
          };
        })([[new J(v.value0)], []])([MR.create, ML.create]))(append13(map22(function(k1) {
          return append13([new ML(v.value0)])(append13(k1)([new MR(v.value0)]));
        })([[new J(v.value0)], []]))(append13(map22(function(k1) {
          return append13([new A(v.value0)])(append13(k1)([new MR(v.value0)]));
        })([[new J(v.value0)], []]))(append13(map22(function(k1) {
          return append13([new ML(v.value0)])(append13(k1)([new A(v.value0)]));
        })([[new J(v.value0)], []]))(append13(map22(function(k1) {
          return append13(k1)([Eps.value]);
        })([[new A(v.value0)], []]))(function() {
          var $1262 = commutative2(v.value0);
          if ($1262) {
            return append13([[new MR(v.value0), new A(v.value0)]])(append13([[new A(v.value0), new ML(v.value0)]])(append13(map22(function(k1) {
              return append13([new MR(v.value0)])(append13(k1)([new ML(v.value0)]));
            })([[new J(v.value0)], []]))(map22(function(k1) {
              return append13([new A(v.value0)])(append13(k1)([new A(v.value0)]));
            })([[new J(v.value0)], []]))));
          }
          ;
          return [];
        }()))))));
      }
      ;
      return true;
    };
  };
  var combineFs = function(v) {
    return function(v1) {
      if (v instanceof S && v1 instanceof S) {
        return pure6(S.value);
      }
      ;
      if (v instanceof R && (v1 instanceof R && eq3(v.value0)(v1.value0))) {
        return pure6(new R(v.value0));
      }
      ;
      if (v instanceof W && (v1 instanceof W && eq3(v.value0)(v1.value0))) {
        return pure6(new W(v.value0));
      }
      ;
      if (v instanceof C && (v1 instanceof C && eq3(v.value1)(v1.value0))) {
        return pure6(new C(v.value0, v1.value1));
      }
      ;
      if (v instanceof D && (v1 instanceof D && eq3(v.value1)(v1.value0))) {
        return pure6(new D(v.value0, v1.value1));
      }
      ;
      return Nil.value;
    };
  };
  var c = /* @__PURE__ */ make_var("c");
  var b = /* @__PURE__ */ make_var("b");
  var atomicTypes = /* @__PURE__ */ function() {
    return new Cons(E.value, new Cons(T.value, new Cons(G.value, Nil.value)));
  }();
  var appl = function(v) {
    if (v instanceof W) {
      return functor(v) && monoid(v.value0);
    }
    ;
    return functor(v) && true;
  };
  var monad = function(f) {
    return appl(f) && true;
  };
  var adjoint = function(v) {
    return function(v1) {
      if (v instanceof W && v1 instanceof R) {
        return eq3(v.value0)(v1.value0);
      }
      ;
      return false;
    };
  };
  var a2 = /* @__PURE__ */ make_var("a");
  var fmapTerm = function(v) {
    if (v instanceof S) {
      return lam(k)(lam(m)(set2(set$prime(lam(a2)(new App2(k, new App2(get_rng(m), a2))))(get_dom(m)))));
    }
    ;
    if (v instanceof R) {
      return lam(k)(lam(m)(lam(g)(new App2(k, new App2(m, g)))));
    }
    ;
    if (v instanceof W) {
      return lam(k)(lam(m)(new Pair(_1(m), new App2(k, _2(m)))));
    }
    ;
    if (v instanceof C) {
      return lam(k)(lam(m)(lam(c)(new App2(m, lam(a2)(new App2(c, new App2(k, a2)))))));
    }
    ;
    if (v instanceof D) {
      return lam(k)(lam(m)(lam(g)(new App2(new App2(fmapTerm(S.value), lam(p3)(new Pair(new App2(k, _1(p3)), _2(p3)))), new App2(m, g)))));
    }
    ;
    return lam(k)(lam(m)(new App2(new App2(make_con("fmap"), k), m)));
  };
  var joinTerm = function(v) {
    if (v instanceof S) {
      return lam(mm)(conc(mm));
    }
    ;
    if (v instanceof R) {
      return lam(mm)(lam(g)(new App2(new App2(mm, g), g)));
    }
    ;
    if (v instanceof W) {
      return lam(mm)(new Pair(mplusTerm(v.value0)(_1(mm))(_1(_2(mm))), _2(_2(mm))));
    }
    ;
    if (v instanceof C) {
      return lam(mm)(lam(c)(new App2(mm, lam(m)(new App2(m, c)))));
    }
    ;
    if (v instanceof D) {
      return lam(mm)(lam(g)(conc(new App2(new App2(fmapTerm(S.value), lam(p3)(new App2(_1(p3), _2(p3)))), new App2(mm, g)))));
    }
    ;
    return lam(mm)(new App2(make_con("join"), mm));
  };
  var pureTerm = function(v) {
    if (v instanceof S) {
      return lam(a2)(set2(set$prime(lam(b)(b))(new App2(make_con("singleton"), a2))));
    }
    ;
    if (v instanceof R) {
      return lam(a2)(lam(g)(a2));
    }
    ;
    if (v instanceof W) {
      return lam(a2)(new Pair(mzeroTerm(v.value0), a2));
    }
    ;
    if (v instanceof C) {
      return lam(a2)(lam(k)(new App2(k, a2)));
    }
    ;
    if (v instanceof D) {
      return lam(a2)(lam(g)(new App2(pureTerm(S.value), new Pair(a2, g))));
    }
    ;
    return lam(a2)(new App2(make_con("pure"), a2));
  };
  var opTerm = function(v) {
    if (v instanceof FA) {
      return lam(l)(lam(r)(new App2(l, r)));
    }
    ;
    if (v instanceof BA) {
      return lam(l)(lam(r)(new App2(r, l)));
    }
    ;
    if (v instanceof PM) {
      return lam(l)(lam(r)(lam(a2)(new App2(new App2(make_con("and"), new App2(l, a2)), new App2(r, a2)))));
    }
    ;
    if (v instanceof FC) {
      return lam(l)(lam(r)(lam(a2)(new App2(l, new App2(r, a2)))));
    }
    ;
    if (v instanceof MR) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(fmapTerm(v.value0), lam(a2)(new App2(new App2(op, l), a2))), r))));
    }
    ;
    if (v instanceof ML) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(fmapTerm(v.value0), lam(a2)(new App2(new App2(op, a2), r))), l))));
    }
    ;
    if (v instanceof UL) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(op, l), lam(a2)(new App2(r, new App2(pureTerm(v.value0), a2)))))));
    }
    ;
    if (v instanceof UR) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(op, lam(a2)(new App2(l, new App2(pureTerm(v.value0), a2)))), r))));
    }
    ;
    if (v instanceof A) {
      return lam(op)(lam(l)(lam(r)(new App2(joinTerm(v.value0), new App2(new App2(fmapTerm(v.value0), lam(a2)(new App2(new App2(fmapTerm(v.value0), new App2(op, a2)), r))), l)))));
    }
    ;
    if (v instanceof Z) {
      return lam(op)(lam(l)(lam(r)(lam(g)(new App2(new App2(op, lam(a2)(new App2(new App2(l, a2), g))), new App2(r, g))))));
    }
    ;
    if (v instanceof J) {
      return lam(op)(lam(l)(lam(r)(new App2(joinTerm(v.value0), new App2(new App2(op, l), r)))));
    }
    ;
    if (v instanceof Eps) {
      return lam(op)(lam(l)(lam(r)(new App2(counitTerm, new App2(new App2(fmapTerm(new W(E.value)), lam(a2)(new App2(new App2(fmapTerm(new R(E.value)), new App2(op, a2)), r))), l)))));
    }
    ;
    if (v instanceof DN) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(new App2(op, l), r), lam(a2)(a2)))));
    }
    ;
    if (v instanceof XL) {
      return lam(op)(lam(l)(lam(r)(new App2(new App2(extendTerm(v.value0), lam(l$prime)(new App2(new App2(new App2(opTerm(v.value1), op), l$prime), r))), l))));
    }
    ;
    throw new Error("Failed pattern match at TDParseCFG (line 463, column 10 - line 504, column 74): " + [v.constructor.name]);
  };
  var addA = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          var v1 = new Tuple(v.value0, v.value1);
          if (v1.value0 instanceof Eff && (v1.value1 instanceof Eff && appl(v1.value0.value0))) {
            return mapFlipped12(combine(new Tuple(v1.value0.value1, v1.value1.value1)))(lift21(function(h) {
              return function(v2) {
                var m1 = new A(h);
                return new Tuple(new Cons(m1, v2.value0), new Tuple(new App2(opTerm(m1), v2.value1.value0), new Eff(h, v2.value1.value1)));
              };
            })(combineFs(v1.value0.value0)(v1.value1.value0)));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addD = function(v) {
    var v1 = function(v2) {
      return Nil.value;
    };
    if (v.value1.value1 instanceof Eff && v.value1.value1.value0 instanceof C) {
      var $1331 = eq3(v.value1.value1.value0.value1)(v.value1.value1.value1);
      if ($1331) {
        var $1332 = norm(v.value0)(DN.value);
        if ($1332) {
          return pure6(new Tuple(new Cons(DN.value, v.value0), new Tuple(new App2(opTerm(DN.value), v.value1.value0), v.value1.value1.value0.value0)));
        }
        ;
        return v1(true);
      }
      ;
      return v1(true);
    }
    ;
    return v1(true);
  };
  var addEps = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          var v1 = new Tuple(v.value0, v.value1);
          if (v1.value0 instanceof Eff && (v1.value1 instanceof Eff && adjoint(v1.value0.value0)(v1.value1.value0))) {
            return mapFlipped12(combine(new Tuple(v1.value0.value1, v1.value1.value1)))(concatMap2(function(v2) {
              return bind6(new Cons(new Tuple(Eps.value, identity7), new Cons(new Tuple(new XL(v1.value0.value0, Eps.value), Eff.create(v1.value0.value0)), Nil.value)))(function(v3) {
                return pure6(new Tuple(new Cons(v3.value0, v2.value0), new Tuple(new App2(opTerm(v3.value0), v2.value1.value0), v3.value1(v2.value1.value1))));
              });
            }));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addJ = function(v) {
    var v1 = function(v2) {
      return Nil.value;
    };
    if (v.value1.value1 instanceof Eff && v.value1.value1.value1 instanceof Eff) {
      var $1361 = monad(v.value1.value1.value0);
      if ($1361) {
        var $1362 = norm(v.value0)(new J(v.value1.value1.value0));
        if ($1362) {
          return mapFlipped2(combineFs(v.value1.value1.value0)(v.value1.value1.value1.value0))(function(h) {
            return new Tuple(new Cons(new J(h), v.value0), new Tuple(new App2(opTerm(new J(h)), v.value1.value0), new Eff(h, v.value1.value1.value1.value1)));
          });
        }
        ;
        return v1(true);
      }
      ;
      return v1(true);
    }
    ;
    return v1(true);
  };
  var addML = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          if (v.value0 instanceof Eff && functor(v.value0.value0)) {
            return mapFlipped12(combine(new Tuple(v.value0.value1, v.value1)))(map10(function(v1) {
              return new Tuple(new Cons(new ML(v.value0.value0), v1.value0), new Tuple(new App2(opTerm(new ML(v.value0.value0)), v1.value1.value0), new Eff(v.value0.value0, v1.value1.value1)));
            }));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addMR = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          if (v.value1 instanceof Eff && functor(v.value1.value0)) {
            return mapFlipped12(combine(new Tuple(v.value0, v.value1.value1)))(map10(function(v1) {
              return new Tuple(new Cons(new MR(v.value1.value0), v1.value0), new Tuple(new App2(opTerm(new MR(v.value1.value0)), v1.value1.value0), new Eff(v.value1.value0, v1.value1.value1)));
            }));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addUL = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          if (v.value1 instanceof Arr && (v.value1.value0 instanceof Eff && appl(v.value1.value0.value0))) {
            return mapFlipped12(combine(new Tuple(v.value0, new Arr(v.value1.value0.value1, v.value1.value1))))(concatMap2(function(v1) {
              var m1 = new UL(v.value1.value0.value0);
              return applySecond2(guard3(norm(v1.value0)(m1)))(pure6(new Tuple(new Cons(m1, v1.value0), new Tuple(new App2(opTerm(m1), v1.value1.value0), v1.value1.value1))));
            }));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addUR = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          if (v.value0 instanceof Arr && (v.value0.value0 instanceof Eff && appl(v.value0.value0.value0))) {
            return mapFlipped12(combine(new Tuple(new Arr(v.value0.value0.value1, v.value0.value1), v.value1)))(concatMap2(function(v1) {
              var m1 = new UR(v.value0.value0.value0);
              return applySecond2(guard3(norm(v1.value0)(m1)))(pure6(new Tuple(new Cons(m1, v1.value0), new Tuple(new App2(opTerm(m1), v1.value1.value0), v1.value1.value1))));
            }));
          }
          ;
          return pure32(Nil.value);
        };
      };
    };
  };
  var addZ = function(dictFunctor) {
    var mapFlipped12 = mapFlipped(dictFunctor);
    return function(dictApplicative) {
      var pure32 = pure(dictApplicative);
      return function(combine) {
        return function(v) {
          var v1 = new Tuple(v.value0, v.value1);
          var v2 = function(v3) {
            return pure32(Nil.value);
          };
          if (v1.value0 instanceof Arr && (v1.value0.value1 instanceof Arr && (v1.value1 instanceof Eff && v1.value1.value0 instanceof R))) {
            var $1426 = eq3(v1.value0.value1.value0)(v1.value1.value0.value0);
            if ($1426) {
              var $1427 = eq3(v1.value1.value1)(v1.value0.value0);
              if ($1427) {
                return mapFlipped12(combine(new Tuple(new Arr(v1.value0.value0, v1.value0.value1.value1), v1.value1.value1)))(map10(function(v3) {
                  return new Tuple(new Cons(Z.value, v3.value0), new Tuple(new App2(opTerm(Z.value), v3.value1.value0), new Arr(v1.value0.value1.value0, v3.value1.value1)));
                }));
              }
              ;
              return v2(true);
            }
            ;
            return v2(true);
          }
          ;
          return v2(true);
        };
      };
    };
  };
  var modes = function(v) {
    return function(v1) {
      if (v instanceof Arr && eq3(v.value0)(v1)) {
        return pure6(new Tuple(pure6(FA.value), new Tuple(opTerm(FA.value), v.value1)));
      }
      ;
      if (v1 instanceof Arr && eq3(v)(v1.value0)) {
        return pure6(new Tuple(pure6(BA.value), new Tuple(opTerm(BA.value), v1.value1)));
      }
      ;
      if (v instanceof Arr && (v.value1 instanceof T && (v1 instanceof Arr && (v1.value1 instanceof T && eq3(v.value0)(v1.value0))))) {
        return pure6(new Tuple(pure6(PM.value), new Tuple(opTerm(PM.value), new Arr(v.value0, T.value))));
      }
      ;
      return Nil.value;
    };
  };
  var openCombine = function(dictFunctor) {
    var map42 = map(dictFunctor);
    return function(dictApply) {
      var flippedApply2 = flippedApply(dictApply)(applyList);
      var apmplus2 = apmplus(dictApply)(semigroupList);
      return function(dictFoldable) {
        var foldl6 = foldl(dictFoldable);
        return function(dictApplicative) {
          var pure32 = pure(dictApplicative);
          return function(bins) {
            return function(uns) {
              return function(combine) {
                return function(v) {
                  return map42(function() {
                    var $1493 = map10(function(v1) {
                      return new Tuple(v1.value0, new Tuple($$eval(v1.value1.value0), v1.value1.value1));
                    });
                    return function($1494) {
                      return $1493(concat2($1494));
                    };
                  }())(flippedApply2(foldl6(function(ls) {
                    return function(k1) {
                      return apmplus2(ls)(k1(combine)(new Tuple(v.value0, v.value1)));
                    };
                  })(pure32(modes(v.value0)(v.value1)))(bins))(pure32(new Cons(pure6, uns))));
                };
              };
            };
          };
        };
      };
    };
  };
  var openCombine1 = /* @__PURE__ */ openCombine(functorStateT2)(/* @__PURE__ */ applyStateT(monadIdentity));
  var combineWith = function(dictOrd) {
    var memoizeTag2 = memoizeTag(dictOrd)(ordTuple2);
    return function(dictFoldable) {
      var openCombine2 = openCombine1(dictFoldable)(applicativeStateT2);
      return function(tag) {
        return function(handler) {
          return function(bins) {
            return function(uns) {
              return curry(fix4(function() {
                var $1495 = memoizeTag2(tag);
                var $1496 = openCombine2(bins)(uns);
                return function($1497) {
                  return $1495(function(v) {
                    var $1498 = map32(handler);
                    return function($1499) {
                      return $1498(v($1499));
                    };
                  }($1496($1497)));
                };
              }()));
            };
          };
        };
      };
    };
  };
  var combineWith1 = /* @__PURE__ */ combineWith(ordBoolean);
  var synsem = function(dictFoldable) {
    var combineWith2 = combineWith1(dictFoldable);
    return function(bins) {
      return function(uns) {
        var goWith = function(tag) {
          return function(handler) {
            return function(l1) {
              return function(r1) {
                return bind32(go(l1))(function(lefts) {
                  return bind32(go(r1))(function(rights) {
                    return map32(concat2)(sequence2(bind6(lefts)(function(v) {
                      return bind6(rights)(function(v1) {
                        return pure6(bind32(combineWith2(tag)(handler)(bins)(uns)(v.value2)(v1.value2))(function(combos) {
                          return pure22(bind6(combos)(function(v2) {
                            var cval = new Comb(v2.value0, $$eval(new App2(new App2(v2.value1.value0, semTerm(v.value1)), semTerm(v1.value1))));
                            return pure6(new Proof(v.value0 + (" " + v1.value0), cval, v2.value1.value1, new Cons(v, new Cons(v1, Nil.value))));
                          }));
                        }));
                      });
                    })));
                  });
                });
              };
            };
          };
        };
        var go = function(v) {
          if (v instanceof Leaf2) {
            return pure22(singleton4(new Proof(v.value0, new Lex(v.value1), v.value2, Nil.value)));
          }
          ;
          if (v instanceof Branch) {
            return goWith(false)(identity7)(v.value0)(v.value1);
          }
          ;
          if (v instanceof Island) {
            return goWith(true)(filter2(function(v1) {
              return evaluated(v1.value1.value1);
            }))(v.value0)(v.value1);
          }
          ;
          throw new Error("Failed pattern match at TDParseCFG (line 268, column 5 - line 268, column 63): " + [v.constructor.name]);
        };
        return function($1500) {
          return execute(go($1500));
        };
      };
    };
  };
  var prove = function(dictFoldable) {
    var synsem1 = synsem(dictFoldable);
    return function(cfg) {
      return function(lex) {
        return function(bins) {
          return function(uns) {
            return function(input3) {
              return map1(concatMap2(synsem1(bins)(uns)))(parse(cfg)(lex)(input3));
            };
          };
        };
      };
    };
  };

  // output/Lexicon.Demo/index.js
  var map11 = /* @__PURE__ */ map(functorList);
  var pure7 = /* @__PURE__ */ pure(applicativeList);
  var append4 = /* @__PURE__ */ append(semigroupList);
  var demoLex = /* @__PURE__ */ function() {
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(new App2(make_con("cat"), make_con("s")), make_con("s"));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(new App2(make_con("ling"), mary), mary);
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map11(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var ann = make_con("a");
    return map11(mkLex)(new Cons(new Tuple("ann", pure7(new Tuple(new Just(ann), new Tuple(DP.value, E.value)))), new Cons(new Tuple("mary", pure7(new Tuple(new Just(mref), new Tuple(DP.value, effW(E.value)(E.value))))), new Cons(new Tuple("marianne", append4(pure7(new Tuple(new Just(ma), new Tuple(DP.value, E.value))))(pure7(new Tuple(new Just(maref), new Tuple(DP.value, effW(E.value)(E.value)))))), new Cons(new Tuple("'s", append4(pure7(new Tuple(new Just(poss), new Tuple(GenD.value, new Arr(E.value, new Arr(new Arr(E.value, E.value), E.value))))))(pure7(new Tuple(new Just(poss$prime), new Tuple(GenD.value, new Arr(E.value, new Arr(new Arr(E.value, E.value), effW(E.value)(E.value)))))))), new Cons(new Tuple("left", pure7(new Tuple(Nothing.value, new Tuple(VP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("whistled", pure7(new Tuple(Nothing.value, new Tuple(VP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("saw", pure7(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("saved", pure7(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("spent", pure7(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("chased", pure7(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("said", pure7(new Tuple(Nothing.value, new Tuple(AV.value, new Arr(T.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("gave", pure7(new Tuple(Nothing.value, new Tuple(DV.value, new Arr(E.value, new Arr(E.value, new Arr(E.value, T.value))))))), new Cons(new Tuple("she", pure7(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value))))), new Cons(new Tuple("it", append4(pure7(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value)))))(pure7(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(effR(E.value)(E.value))(effR(E.value)(E.value))))))), new Cons(new Tuple("her", append4(pure7(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value)))))(pure7(new Tuple(new Just(idTerm), new Tuple(Gen.value, effR(E.value)(E.value)))))), new Cons(new Tuple("she2", pure7(new Tuple(new Just(pushTerm), new Tuple(DP.value, effR(E.value)(effW(E.value)(E.value)))))), new Cons(new Tuple("her2", append4(pure7(new Tuple(new Just(pushTerm), new Tuple(DP.value, effR(E.value)(effW(E.value)(E.value))))))(append4(pure7(new Tuple(new Just(pushTerm), new Tuple(Gen.value, effR(E.value)(effW(E.value)(E.value))))))(append4(pure7(new Tuple(new Just(pc$prime), new Tuple(Gen.value, new Arr(new Arr(E.value, E.value), effR(E.value)(effW(E.value)(E.value)))))))(pure7(new Tuple(new Just(pc), new Tuple(Gen.value, new Arr(new Arr(E.value, E.value), effR(E.value)(effW(effR(E.value)(E.value))(E.value)))))))))), new Cons(new Tuple("mom", pure7(new Tuple(Nothing.value, new Tuple(FN.value, new Arr(E.value, E.value))))), new Cons(new Tuple("paycheck", pure7(new Tuple(Nothing.value, new Tuple(FN.value, new Arr(E.value, E.value))))), new Cons(new Tuple("pictureof", pure7(new Tuple(Nothing.value, new Tuple(RN.value, new Arr(E.value, new Arr(E.value, E.value)))))), new Cons(new Tuple("the", pure7(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), E.value))))), new Cons(new Tuple("theC", pure7(new Tuple(Nothing.value, new Tuple(Det.value, effC(E.value)(T.value)(E.value))))), new Cons(new Tuple("very", pure7(new Tuple(Nothing.value, new Tuple(Deg.value, new Arr(new Arr(E.value, T.value), new Arr(E.value, T.value)))))), new Cons(new Tuple("every", pure7(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effC(T.value)(T.value)(E.value)))))), new Cons(new Tuple("everyP", pure7(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), new Arr(new Arr(E.value, T.value), T.value)))))), new Cons(new Tuple("everyC", pure7(new Tuple(Nothing.value, new Tuple(Det.value, effC(effC(T.value)(T.value)(E.value))(T.value)(E.value))))), new Cons(new Tuple("big", pure7(new Tuple(Nothing.value, new Tuple(AdjP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("happy", pure7(new Tuple(Nothing.value, new Tuple(AdjP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("dog", pure7(new Tuple(Nothing.value, new Tuple(NP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("cat", pure7(new Tuple(Nothing.value, new Tuple(NP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("near", pure7(new Tuple(Nothing.value, new Tuple(TAdj.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("some", pure7(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effS(E.value)))))), new Cons(new Tuple("some2", pure7(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effS(effW(E.value)(E.value))))))), new Cons(new Tuple("someone", pure7(new Tuple(Nothing.value, new Tuple(DP.value, effC(T.value)(T.value)(E.value))))), new Cons(new Tuple("someone2", pure7(new Tuple(new Just(so2), new Tuple(DP.value, effS(effW(E.value)(E.value)))))), new Cons(new Tuple("someone3", pure7(new Tuple(new Just(so3), new Tuple(DP.value, effS(E.value))))), new Cons(new Tuple("everyone", pure7(new Tuple(new Just(eo), new Tuple(DP.value, effC(T.value)(T.value)(E.value))))), new Cons(new Tuple("everyone2", pure7(new Tuple(new Just(eo2), new Tuple(DP.value, effC(T.value)(T.value)(effW(E.value)(E.value)))))), new Cons(new Tuple("tr", pure7(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value))))), new Cons(new Tuple("and", pure7(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(T.value, new Arr(T.value, T.value)))))), new Cons(new Tuple("but", pure7(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(T.value, new Arr(T.value, T.value)))))), new Cons(new Tuple("andE", pure7(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(E.value, new Arr(E.value, E.value)))))), new Cons(new Tuple("with", pure7(new Tuple(Nothing.value, new Tuple(TAdv.value, new Arr(E.value, new Arr(new Arr(E.value, T.value), new Arr(E.value, T.value))))))), new Cons(new Tuple("eclo", append4(pure7(new Tuple(new Just(eclo), new Tuple(Cmp.value, new Arr(effS(T.value), T.value)))))(pure7(new Tuple(new Just(eclo), new Tuple(Dmp.value, new Arr(effS(T.value), T.value)))))), new Cons(new Tuple("maryaling", pure7(new Tuple(new Just(ml), new Tuple(DP.value, effW(T.value)(E.value))))), new Cons(new Tuple("sassyacat", pure7(new Tuple(new Just(sc), new Tuple(DP.value, effW(T.value)(E.value))))), Nil.value)))))))))))))))))))))))))))))))))))))))))))))));
  }();

  // output/Lexicon.Dyn/index.js
  var set3 = /* @__PURE__ */ set(categoryFn);
  var map12 = /* @__PURE__ */ map(functorList);
  var pure8 = /* @__PURE__ */ pure(applicativeList);
  var dynLex = /* @__PURE__ */ function() {
    var soD = function() {
      var v = new Tuple(make_var("g"), make_var("x"));
      return lam(v.value0)(set3(set$prime(lam(v.value1)(new Pair(v.value1, v.value0)))(make_con("someone"))));
    }();
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var sD = function() {
      var v = new Tuple(make_var("g"), new Tuple(make_var("x"), make_var("p")));
      return lam(v.value1.value1)(lam(v.value0)(set3(set$prime(lam(v.value1.value0)(new Pair(v.value1.value0, v.value0)))(new App2(make_con("some"), v.value1.value1)))));
    }();
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var pD = function() {
      var v = new Tuple(make_var("g"), new Tuple(make_var("x"), make_var("p")));
      return lam(v.value1.value0)(lam(v.value0)(set3(set$prime(lam(v.value1.value1)(new Pair(v.value1.value0, new Push(v.value1.value0, v.value0))))(make_con("_")))));
    }();
    var p22 = function() {
      var v = new Tuple(make_var("g"), make_var("p"));
      return lam(v.value0)(set3(set$prime(lam(v.value1)(new Pair(ix(v.value0)(2), v.value0)))(make_con("_"))));
    }();
    var p12 = function() {
      var v = new Tuple(make_var("g"), make_var("p"));
      return lam(v.value0)(set3(set$prime(lam(v.value1)(new Pair(ix(v.value0)(1), v.value0)))(make_con("_"))));
    }();
    var p0 = function() {
      var v = new Tuple(make_var("g"), make_var("p"));
      return lam(v.value0)(set3(set$prime(lam(v.value1)(new Pair(ix(v.value0)(0), v.value0)))(make_con("_"))));
    }();
    var not3 = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value0)(lam(v.value1)(new App2(make_con("\xAC \u2203"), new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map12(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var no = function() {
      var v = new Tuple(make_var("p"), new Tuple(make_var("x"), make_var("k")));
      return lam(v.value0)(new App2(make_con("nobody"), lam(v.value1.value0)(new App2(eclo, new App2(new App2(fmapTerm(S.value), lam(v.value1.value1)(new App2(v.value1.value1, v.value1.value0))), v.value0)))));
    }();
    var ann = make_con("a");
    return map12(mkLex)(new Cons(new Tuple("push", pure8(new Tuple(new Just(pD), new Tuple(Dmp.value, new Arr(E.value, effD(G.value)(G.value)(E.value)))))), new Cons(new Tuple("pro0", pure8(new Tuple(new Just(p0), new Tuple(DP.value, effD(G.value)(G.value)(E.value))))), new Cons(new Tuple("pro1", pure8(new Tuple(new Just(p12), new Tuple(DP.value, effD(G.value)(G.value)(E.value))))), new Cons(new Tuple("pro2", pure8(new Tuple(new Just(p22), new Tuple(DP.value, effD(G.value)(G.value)(E.value))))), new Cons(new Tuple("someD", pure8(new Tuple(new Just(sD), new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effD(G.value)(G.value)(E.value)))))), new Cons(new Tuple("aD", pure8(new Tuple(new Just(sD), new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effD(G.value)(G.value)(E.value)))))), new Cons(new Tuple("someoneD", pure8(new Tuple(new Just(soD), new Tuple(DP.value, effD(G.value)(G.value)(E.value))))), Nil.value))))))));
  }();

  // output/Lexicon.Indef/index.js
  var map13 = /* @__PURE__ */ map(functorList);
  var pure9 = /* @__PURE__ */ pure(applicativeList);
  var indefLex = /* @__PURE__ */ function() {
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var not3 = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value0)(lam(v.value1)(new App2(make_con("\xAC \u2203"), new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map13(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var no = function() {
      var v = new Tuple(make_var("p"), new Tuple(make_var("x"), make_var("k")));
      return lam(v.value0)(new App2(make_con("nobody"), lam(v.value1.value0)(new App2(eclo, new App2(new App2(fmapTerm(S.value), lam(v.value1.value1)(new App2(v.value1.value1, v.value1.value0))), v.value0)))));
    }();
    var ann = make_con("a");
    return map13(mkLex)(new Cons(new Tuple("some", pure9(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effS(E.value)))))), new Cons(new Tuple("a", pure9(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effS(E.value)))))), new Cons(new Tuple("someone", pure9(new Tuple(new Just(so3), new Tuple(DP.value, effS(E.value))))), new Cons(new Tuple("nobody", pure9(new Tuple(new Just(no), new Tuple(DP.value, new Arr(effS(new Arr(E.value, T.value)), T.value))))), new Cons(new Tuple("eclo", append(semigroupList)(pure9(new Tuple(new Just(eclo), new Tuple(Cmp.value, new Arr(effS(T.value), T.value)))))(pure9(new Tuple(new Just(eclo), new Tuple(Dmp.value, new Arr(effS(T.value), T.value)))))), Nil.value))))));
  }();

  // output/Lexicon.Pro/index.js
  var map14 = /* @__PURE__ */ map(functorList);
  var pure10 = /* @__PURE__ */ pure(applicativeList);
  var append5 = /* @__PURE__ */ append(semigroupList);
  var proLex = /* @__PURE__ */ function() {
    var top3 = function() {
      var f = make_var("f");
      return lam(f)(f);
    }();
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map14(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var ann = make_con("a");
    return map14(mkLex)(new Cons(new Tuple("she", pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value))))), new Cons(new Tuple("it", append5(pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value)))))(pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(effR(E.value)(E.value))(effR(E.value)(E.value))))))), new Cons(new Tuple("her", append5(pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value)))))(pure10(new Tuple(new Just(idTerm), new Tuple(Gen.value, effR(E.value)(E.value)))))), new Cons(new Tuple("who", pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value))))), new Cons(new Tuple("__", append5(pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(E.value)(E.value)))))(pure10(new Tuple(new Just(idTerm), new Tuple(DP.value, effR(effR(E.value)(E.value))(effR(E.value)(E.value))))))), new Cons(new Tuple(",", append5(pure10(new Tuple(new Just(top3), new Tuple(AV.value, new Arr(effR(E.value)(T.value), new Arr(E.value, T.value))))))(append5(pure10(new Tuple(new Just(top3), new Tuple(AV.value, new Arr(effR(effR(E.value)(E.value))(T.value), new Arr(effR(E.value)(E.value), T.value))))))(pure10(new Tuple(new Just(top3), new Tuple(AV.value, new Arr(effR(effR(E.value)(E.value))(effR(E.value)(T.value)), new Arr(effR(E.value)(E.value), effR(E.value)(T.value))))))))), new Cons(new Tuple(":", append5(pure10(new Tuple(new Just(top3), new Tuple(AV.value, new Arr(effR(E.value)(T.value), new Arr(E.value, T.value))))))(pure10(new Tuple(new Just(top3), new Tuple(AV.value, new Arr(effR(effR(E.value)(E.value))(T.value), new Arr(effR(E.value)(E.value), T.value))))))), Nil.value))))))));
  }();

  // output/Lexicon.Pure/index.js
  var map15 = /* @__PURE__ */ map(functorList);
  var pure11 = /* @__PURE__ */ pure(applicativeList);
  var pureLex = /* @__PURE__ */ function() {
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map15(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var ann = make_con("a");
    return map15(mkLex)(new Cons(new Tuple("ann", pure11(new Tuple(Nothing.value, new Tuple(DP.value, E.value)))), new Cons(new Tuple("mary", pure11(new Tuple(Nothing.value, new Tuple(DP.value, E.value)))), new Cons(new Tuple("'s", pure11(new Tuple(new Just(poss), new Tuple(GenD.value, new Arr(E.value, new Arr(new Arr(E.value, E.value), E.value)))))), new Cons(new Tuple("left", pure11(new Tuple(Nothing.value, new Tuple(VP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("whistled", pure11(new Tuple(Nothing.value, new Tuple(VP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("saw", pure11(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("saved", pure11(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("spent", pure11(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("chased", pure11(new Tuple(Nothing.value, new Tuple(TV.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("said", pure11(new Tuple(Nothing.value, new Tuple(AV.value, new Arr(T.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("gave", pure11(new Tuple(Nothing.value, new Tuple(DV.value, new Arr(E.value, new Arr(E.value, new Arr(E.value, T.value))))))), new Cons(new Tuple("mom", pure11(new Tuple(Nothing.value, new Tuple(FN.value, new Arr(E.value, E.value))))), new Cons(new Tuple("paycheck", pure11(new Tuple(Nothing.value, new Tuple(FN.value, new Arr(E.value, E.value))))), new Cons(new Tuple("pictureof", pure11(new Tuple(Nothing.value, new Tuple(RN.value, new Arr(E.value, new Arr(E.value, E.value)))))), new Cons(new Tuple("the", pure11(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), E.value))))), new Cons(new Tuple("very", pure11(new Tuple(Nothing.value, new Tuple(Deg.value, new Arr(new Arr(E.value, T.value), new Arr(E.value, T.value)))))), new Cons(new Tuple("everyP", pure11(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), new Arr(new Arr(E.value, T.value), T.value)))))), new Cons(new Tuple("big", pure11(new Tuple(Nothing.value, new Tuple(AdjP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("happy", pure11(new Tuple(Nothing.value, new Tuple(AdjP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("dog", pure11(new Tuple(Nothing.value, new Tuple(NP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("cat", pure11(new Tuple(Nothing.value, new Tuple(NP.value, new Arr(E.value, T.value))))), new Cons(new Tuple("near", pure11(new Tuple(Nothing.value, new Tuple(TAdj.value, new Arr(E.value, new Arr(E.value, T.value)))))), new Cons(new Tuple("and", pure11(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(T.value, new Arr(T.value, T.value)))))), new Cons(new Tuple("but", pure11(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(T.value, new Arr(T.value, T.value)))))), new Cons(new Tuple("if", pure11(new Tuple(Nothing.value, new Tuple(Adc.value, new Arr(T.value, new Arr(T.value, T.value)))))), new Cons(new Tuple("andE", pure11(new Tuple(Nothing.value, new Tuple(Cor.value, new Arr(E.value, new Arr(E.value, E.value)))))), new Cons(new Tuple("with", pure11(new Tuple(Nothing.value, new Tuple(TAdv.value, new Arr(E.value, new Arr(new Arr(E.value, T.value), new Arr(E.value, T.value))))))), Nil.value))))))))))))))))))))))))))));
  }();

  // output/Lexicon.Push/index.js
  var map16 = /* @__PURE__ */ map(functorList);
  var pure12 = /* @__PURE__ */ pure(applicativeList);
  var append6 = /* @__PURE__ */ append(semigroupList);
  var pushLex = /* @__PURE__ */ function() {
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map16(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var ann = make_con("a");
    return map16(mkLex)(new Cons(new Tuple("mary", pure12(new Tuple(new Just(mref), new Tuple(DP.value, effW(E.value)(E.value))))), new Cons(new Tuple("marianne", append6(pure12(new Tuple(new Just(ma), new Tuple(DP.value, E.value))))(pure12(new Tuple(new Just(maref), new Tuple(DP.value, effW(E.value)(E.value)))))), new Cons(new Tuple("'s", pure12(new Tuple(new Just(poss$prime), new Tuple(GenD.value, new Arr(E.value, new Arr(new Arr(E.value, E.value), effW(E.value)(E.value))))))), new Cons(new Tuple("she2", pure12(new Tuple(new Just(pushTerm), new Tuple(DP.value, effR(E.value)(effW(E.value)(E.value)))))), new Cons(new Tuple("her2", append6(pure12(new Tuple(new Just(pushTerm), new Tuple(DP.value, effR(E.value)(effW(E.value)(E.value))))))(append6(pure12(new Tuple(new Just(pushTerm), new Tuple(Gen.value, effR(E.value)(effW(E.value)(E.value))))))(append6(pure12(new Tuple(new Just(pc$prime), new Tuple(Gen.value, new Arr(new Arr(E.value, E.value), effR(E.value)(effW(E.value)(E.value)))))))(pure12(new Tuple(new Just(pc), new Tuple(Gen.value, new Arr(new Arr(E.value, E.value), effR(E.value)(effW(effR(E.value)(E.value))(E.value)))))))))), new Cons(new Tuple("some2", pure12(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effS(effW(E.value)(E.value))))))), new Cons(new Tuple("someone2", pure12(new Tuple(new Just(so2), new Tuple(DP.value, effS(effW(E.value)(E.value)))))), new Cons(new Tuple("everyone2", pure12(new Tuple(new Just(eo2), new Tuple(DP.value, effC(T.value)(T.value)(effW(E.value)(E.value)))))), Nil.value)))))))));
  }();

  // output/Lexicon.Quant/index.js
  var map17 = /* @__PURE__ */ map(functorList);
  var pure13 = /* @__PURE__ */ pure(applicativeList);
  var quantLex = /* @__PURE__ */ function() {
    var so3 = make_set(make_con("person"));
    var second = function(v) {
      return function(f) {
        return new Tuple(v.value0, f(v.value0)(v.value1));
      };
    };
    var sc = new Pair(make_var("s"), new App2(make_var("cat"), make_var("s")));
    var pushTerm = function() {
      var x2 = make_var("x");
      return lam(x2)(new Pair(x2, x2));
    }();
    var so2 = new App2(new App2(fmapTerm(S.value), pushTerm), so3);
    var poss$prime = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var poss = function() {
      var v = new Tuple(make_var("p"), make_var("x"));
      return lam(v.value1)(lam(v.value0)(new App2(v.value0, v.value1)));
    }();
    var pc$prime = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new App2(pushTerm, new App2(v.value0, v.value1))));
    }();
    var pc = function() {
      var v = new Tuple(make_var("p"), make_var("g"));
      return lam(v.value0)(lam(v.value1)(new Pair(v.value0, new App2(v.value0, v.value1))));
    }();
    var mary = make_con("m");
    var ml = new Pair(mary, new App2(make_var("ling"), mary));
    var mref = new App2(pushTerm, mary);
    var ma = make_con("ma");
    var maref = new App2(pushTerm, ma);
    var idTerm = function() {
      var a3 = make_var("a");
      return lam(a3)(a3);
    }();
    var first = function(v) {
      return function(f) {
        return new Tuple(f(v.value0), v.value1);
      };
    };
    var mkLex = function(w) {
      return second(w)(function(s) {
        return map17(function(v) {
          return first(v)(fromMaybe(make_con(s)));
        });
      });
    };
    var eo = make_con("everyone");
    var eo2 = new App2(new App2(fmapTerm(new C(T.value, T.value)), pushTerm), eo);
    var eclo = make_con("\u2203");
    var ann = make_con("a");
    return map17(mkLex)(new Cons(new Tuple("theC", pure13(new Tuple(Nothing.value, new Tuple(Det.value, effC(E.value)(T.value)(E.value))))), new Cons(new Tuple("every", pure13(new Tuple(Nothing.value, new Tuple(Det.value, new Arr(new Arr(E.value, T.value), effC(T.value)(T.value)(E.value)))))), new Cons(new Tuple("everyC", pure13(new Tuple(Nothing.value, new Tuple(Det.value, effC(effC(T.value)(T.value)(E.value))(T.value)(E.value))))), new Cons(new Tuple("someone", pure13(new Tuple(Nothing.value, new Tuple(DP.value, effC(T.value)(T.value)(E.value))))), new Cons(new Tuple("everyone", pure13(new Tuple(new Just(eo), new Tuple(DP.value, effC(T.value)(T.value)(E.value))))), Nil.value))))));
  }();

  // output/TDDemo/index.js
  var pure14 = /* @__PURE__ */ pure(applicativeList);
  var demoCFG = function(v) {
    return function(v1) {
      if (v instanceof DP && v1 instanceof VP) {
        return pure14(CP.value);
      }
      ;
      if (v instanceof Cmp && v1 instanceof CP) {
        return pure14(CP.value);
      }
      ;
      if (v instanceof CP && v1 instanceof CBar) {
        return pure14(CP.value);
      }
      ;
      if (v instanceof Cor && v1 instanceof CP) {
        return pure14(CBar.value);
      }
      ;
      if (v instanceof CP && v1 instanceof AdcP) {
        return pure14(CP.value);
      }
      ;
      if (v instanceof AdcP && v1 instanceof CP) {
        return pure14(CP.value);
      }
      ;
      if (v instanceof Adc && v1 instanceof CP) {
        return pure14(AdcP.value);
      }
      ;
      if (v instanceof Det && v1 instanceof NP) {
        return pure14(DP.value);
      }
      ;
      if (v instanceof Gen && v1 instanceof FN) {
        return pure14(DP.value);
      }
      ;
      if (v instanceof RN && v1 instanceof DP) {
        return pure14(FN.value);
      }
      ;
      if (v instanceof Dmp && v1 instanceof DP) {
        return pure14(DP.value);
      }
      ;
      if (v instanceof DP && v1 instanceof DBar) {
        return pure14(DP.value);
      }
      ;
      if (v instanceof DP && v1 instanceof GenD) {
        return pure14(Gen.value);
      }
      ;
      if (v instanceof Cor && v1 instanceof DP) {
        return pure14(DBar.value);
      }
      ;
      if (v instanceof AdjP && v1 instanceof NP) {
        return pure14(NP.value);
      }
      ;
      if (v instanceof NP && v1 instanceof AdjP) {
        return pure14(NP.value);
      }
      ;
      if (v instanceof TAdj && v1 instanceof DP) {
        return pure14(AdjP.value);
      }
      ;
      if (v instanceof Deg && v1 instanceof AdjP) {
        return pure14(AdjP.value);
      }
      ;
      if (v instanceof TV && v1 instanceof DP) {
        return pure14(VP.value);
      }
      ;
      if (v instanceof AV && v1 instanceof CP) {
        return pure14(VP.value);
      }
      ;
      if (v instanceof VP && v1 instanceof AdvP) {
        return pure14(VP.value);
      }
      ;
      if (v instanceof DV && v1 instanceof DP) {
        return pure14(TV.value);
      }
      ;
      if (v instanceof TAdv && v1 instanceof DP) {
        return pure14(AdvP.value);
      }
      ;
      return Nil.value;
    };
  };

  // output/Parsing/index.js
  var $runtime_lazy5 = function(name2, moduleName, init4) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init4();
      state2 = 2;
      return val;
    };
  };
  var unwrap4 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new ParseState2(value0, value1, value2);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift2(value0) {
      this.value0 = value0;
    }
    ;
    Lift2.create = function(value0) {
      return new Lift2(value0);
    };
    return Lift2;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var lazyParserT = {
    defer: function(f) {
      var m2 = defer2(f);
      return function(state1, more, lift1, $$throw2, done) {
        var v = force(m2);
        return v(state1, more, lift1, $$throw2, done);
      };
    }
  };
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state2, a3) {
              return more(function(v2) {
                return done(state2, f(a3));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state2, f) {
              return more(function(v3) {
                return v1(state2, more, lift1, $$throw2, function(state3, a3) {
                  return more(function(v4) {
                    return done(state3, f(a3));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state2, a3) {
              return more(function(v2) {
                var v3 = next(a3);
                return v3(state2, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a3) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a3);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadRecParserT = {
    tailRecM: function(next) {
      return function(initArg) {
        return function(state1, more, lift1, $$throw2, done) {
          var $lazy_loop = $runtime_lazy5("loop", "Parsing", function() {
            return function(state2, arg, gas) {
              var v = next(arg);
              return v(state2, more, lift1, $$throw2, function(state3, step2) {
                if (step2 instanceof Loop) {
                  var $188 = gas === 0;
                  if ($188) {
                    return more(function(v1) {
                      return $lazy_loop(269)(state3, step2.value0, 30);
                    });
                  }
                  ;
                  return $lazy_loop(271)(state3, step2.value0, gas - 1 | 0);
                }
                ;
                if (step2 instanceof Done) {
                  return done(state3, step2.value0);
                }
                ;
                throw new Error("Failed pattern match at Parsing (line 265, column 39 - line 273, column 43): " + [step2.constructor.name]);
              });
            };
          });
          var loop = $lazy_loop(262);
          return loop(state1, initArg, 30);
        };
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError2 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k2) {
    return function(state1, v, v1, v2, done) {
      var v3 = k2(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map26 = map(Monad0.Bind1().Apply0().Functor0());
    var pure111 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step2) {
            var v1 = step2(unit);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift) {
              $tco_done = true;
              return map26(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure111(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 144, column 13 - line 150, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM4(go)(function(v1) {
          return v(state1, More.create, Lift.create, function(state2, err) {
            return new Stop(state2, new Left(err));
          }, function(state2, res) {
            return new Stop(state2, new Right(res));
          });
        });
      };
    };
  };
  var position = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var parseErrorMessage = function(v) {
    return v.value0;
  };
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map26 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p5) {
        var initialState = new ParseState(s, initialPos, false);
        return map26(fst)(runParserT$prime1(initialState)(p5));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $253 = runParserT1(s);
    return function($254) {
      return unwrap4($253($254));
    };
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError2(new ParseError(message2, pos));
    };
  };
  var fail = function(message2) {
    return bindFlipped3(failWithPosition(message2))(position);
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };

  // output/Parsing.Combinators/index.js
  var alt2 = /* @__PURE__ */ alt(altParserT);
  var defer3 = /* @__PURE__ */ defer(lazyParserT);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorParserT);
  var pure15 = /* @__PURE__ */ pure(applicativeParserT);
  var applySecond3 = /* @__PURE__ */ applySecond(applyParserT);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecParserT);
  var bind7 = /* @__PURE__ */ bind(bindParserT);
  var map18 = /* @__PURE__ */ map(functorParserT);
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var applyFirst2 = /* @__PURE__ */ applyFirst(applyParserT);
  var empty4 = /* @__PURE__ */ empty(plusParserT);
  var withLazyErrorMessage = function(p5) {
    return function(msg) {
      return alt2(p5)(defer3(function(v) {
        return fail("Expected " + msg(unit));
      }));
    };
  };
  var withErrorMessage = function(p5) {
    return function(msg) {
      return alt2(p5)(fail("Expected " + msg));
    };
  };
  var $$try3 = function(v) {
    return function(v1, more, lift3, $$throw2, done) {
      return v(v1, more, lift3, function(v2, err) {
        return $$throw2(new ParseState(v2.value0, v2.value1, v1.value2), err);
      }, done);
    };
  };
  var skipMany1 = function(p5) {
    var go = function(v) {
      return alt2(voidLeft3(p5)(new Loop(unit)))(pure15(new Done(unit)));
    };
    return applySecond3(p5)(tailRecM3(go)(unit));
  };
  var skipMany = function(p5) {
    return alt2(skipMany1(p5))(pure15(unit));
  };
  var sepBy1 = function(p5) {
    return function(sep) {
      return bind7(p5)(function(a3) {
        return bind7(manyRec2(applySecond3(sep)(p5)))(function(as) {
          return pure15(cons$prime(a3)(as));
        });
      });
    };
  };
  var sepBy = function(p5) {
    return function(sep) {
      return alt2(map18(toList)(sepBy1(p5)(sep)))(pure15(Nil.value));
    };
  };
  var option = function(a3) {
    return function(p5) {
      return alt2(p5)(pure15(a3));
    };
  };
  var notFollowedBy = function(p5) {
    return $$try3(alt2(applySecond3($$try3(p5))(fail("Negated parser succeeded")))(pure15(unit)));
  };
  var choice = function(dictFoldable) {
    var go = function(p12) {
      return function(v) {
        if (v instanceof Nothing) {
          return new Just(p12);
        }
        ;
        if (v instanceof Just) {
          return new Just(alt2(p12)(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Combinators (line 357, column 11 - line 359, column 32): " + [v.constructor.name]);
      };
    };
    var $92 = fromMaybe(empty4);
    var $93 = foldr(dictFoldable)(go)(Nothing.value);
    return function($94) {
      return $92($93($94));
    };
  };
  var chainl1 = function(p5) {
    return function(f) {
      var go = function(a3) {
        return alt2(bind7(f)(function(op2) {
          return bind7(p5)(function(a$prime) {
            return pure15(new Loop(op2(a3)(a$prime)));
          });
        }))(pure15(new Done(a3)));
      };
      return bind7(p5)(function(a3) {
        return tailRecM3(go)(a3);
      });
    };
  };
  var between = function(open) {
    return function(close) {
      return function(p5) {
        return applyFirst2(applySecond3(open)(p5))(close);
      };
    };
  };
  var asErrorMessage = /* @__PURE__ */ flip(withErrorMessage);

  // output/Parsing.Expr/index.js
  var bind8 = /* @__PURE__ */ bind(bindParserT);
  var pure16 = /* @__PURE__ */ pure(applicativeParserT);
  var alt3 = /* @__PURE__ */ alt(altParserT);
  var foldr3 = /* @__PURE__ */ foldr(foldableArray);
  var choice2 = /* @__PURE__ */ choice(foldableList);
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var foldl4 = /* @__PURE__ */ foldl(foldableArray);
  var AssocNone = /* @__PURE__ */ function() {
    function AssocNone2() {
    }
    ;
    AssocNone2.value = new AssocNone2();
    return AssocNone2;
  }();
  var AssocLeft = /* @__PURE__ */ function() {
    function AssocLeft2() {
    }
    ;
    AssocLeft2.value = new AssocLeft2();
    return AssocLeft2;
  }();
  var AssocRight = /* @__PURE__ */ function() {
    function AssocRight2() {
    }
    ;
    AssocRight2.value = new AssocRight2();
    return AssocRight2;
  }();
  var Infix = /* @__PURE__ */ function() {
    function Infix2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Infix2.create = function(value0) {
      return function(value1) {
        return new Infix2(value0, value1);
      };
    };
    return Infix2;
  }();
  var Prefix = /* @__PURE__ */ function() {
    function Prefix2(value0) {
      this.value0 = value0;
    }
    ;
    Prefix2.create = function(value0) {
      return new Prefix2(value0);
    };
    return Prefix2;
  }();
  var Postfix = /* @__PURE__ */ function() {
    function Postfix2(value0) {
      this.value0 = value0;
    }
    ;
    Postfix2.create = function(value0) {
      return new Postfix2(value0);
    };
    return Postfix2;
  }();
  var termP = function(prefixP) {
    return function(term) {
      return function(postfixP) {
        return bind8(prefixP)(function(pre) {
          return bind8(term)(function(x2) {
            return bind8(postfixP)(function(post) {
              return pure16(post(pre(x2)));
            });
          });
        });
      };
    };
  };
  var splitOp = function(v) {
    return function(accum) {
      if (v instanceof Infix && v.value1 instanceof AssocNone) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: new Cons(v.value0, accum.nassoc),
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Infix && v.value1 instanceof AssocLeft) {
        return {
          rassoc: accum.rassoc,
          lassoc: new Cons(v.value0, accum.lassoc),
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Infix && v.value1 instanceof AssocRight) {
        return {
          rassoc: new Cons(v.value0, accum.rassoc),
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Prefix) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: new Cons(v.value0, accum.prefix),
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Postfix) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: new Cons(v.value0, accum.postfix)
        };
      }
      ;
      throw new Error("Failed pattern match at Parsing.Expr (line 78, column 1 - line 78, column 80): " + [v.constructor.name, accum.constructor.name]);
    };
  };
  var rassocP1 = function(x2) {
    return function(rassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return alt3(rassocP(x2)(rassocOp)(prefixP)(term)(postfixP))(pure16(x2));
          };
        };
      };
    };
  };
  var rassocP = function(x2) {
    return function(rassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind8(rassocOp)(function(f) {
              return bind8(bind8(termP(prefixP)(term)(postfixP))(function(z) {
                return rassocP1(z)(rassocOp)(prefixP)(term)(postfixP);
              }))(function(y) {
                return pure16(f(x2)(y));
              });
            });
          };
        };
      };
    };
  };
  var nassocP = function(x2) {
    return function(nassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind8(nassocOp)(function(f) {
              return bind8(termP(prefixP)(term)(postfixP))(function(y) {
                return pure16(f(x2)(y));
              });
            });
          };
        };
      };
    };
  };
  var lassocP1 = function(x2) {
    return function(lassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return alt3(lassocP(x2)(lassocOp)(prefixP)(term)(postfixP))(pure16(x2));
          };
        };
      };
    };
  };
  var lassocP = function(x2) {
    return function(lassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind8(lassocOp)(function(f) {
              return bind8(termP(prefixP)(term)(postfixP))(function(y) {
                return lassocP1(f(x2)(y))(lassocOp)(prefixP)(term)(postfixP);
              });
            });
          };
        };
      };
    };
  };
  var makeParser = function(term) {
    return function(ops) {
      var accum = foldr3(splitOp)({
        rassoc: Nil.value,
        lassoc: Nil.value,
        nassoc: Nil.value,
        prefix: Nil.value,
        postfix: Nil.value
      })(ops);
      var lassocOp = choice2(accum.lassoc);
      var nassocOp = choice2(accum.nassoc);
      var postfixOp = withErrorMessage(choice2(accum.postfix))("");
      var postfixP = alt3(postfixOp)(pure16(identity8));
      var prefixOp = withErrorMessage(choice2(accum.prefix))("");
      var prefixP = alt3(prefixOp)(pure16(identity8));
      var rassocOp = choice2(accum.rassoc);
      return bind8(termP(prefixP)(term)(postfixP))(function(x2) {
        return alt3(rassocP(x2)(rassocOp)(prefixP)(term)(postfixP))(alt3(lassocP(x2)(lassocOp)(prefixP)(term)(postfixP))(alt3(nassocP(x2)(nassocOp)(prefixP)(term)(postfixP))(withErrorMessage(pure16(x2))("operator"))));
      });
    };
  };
  var buildExprParser = function(operators) {
    return function(simpleExpr) {
      return foldl4(makeParser)(simpleExpr)(operators);
    };
  };

  // output/Parsing.String/index.js
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show4 = /* @__PURE__ */ show(showString);
  var show22 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum3(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum3(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var updatePosString = function($copy_pos) {
    return function($copy_before) {
      return function($copy_after) {
        var $tco_var_pos = $copy_pos;
        var $tco_var_before = $copy_before;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(pos, before, after) {
          var v = uncons4(before);
          if (v instanceof Nothing) {
            $tco_done = true;
            return pos;
          }
          ;
          if (v instanceof Just) {
            var newPos = function() {
              if ($$null2(v.value0.tail)) {
                return updatePosSingle(pos)(v.value0.head)(after);
              }
              ;
              if (otherwise) {
                return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 160, column 7 - line 162, column 52): " + []);
            }();
            $tco_var_pos = newPos;
            $tco_var_before = v.value0.tail;
            $copy_after = after;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parsing.String (line 156, column 36 - line 163, column 38): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
        }
        ;
        return $tco_result;
      };
    };
  };
  var satisfyCodePoint = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons4(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var $66 = f(v3.value0.head);
                if ($66) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 131, column 7 - line 138, column 73): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons4(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum3(v3.value0.head);
                var $75 = cp < 0 || cp > 65535;
                if ($75) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust5(toEnum2(cp));
                var $76 = f(ch);
                if ($76) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 109, column 7 - line 124, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var consumeWith = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = f(v.value0);
              if (v3 instanceof Left) {
                return $$throw2(v, new ParseError(v3.value0, v.value1));
              }
              ;
              if (v3 instanceof Right) {
                return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 297, column 7 - line 301, column 121): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var string = function(str) {
    return consumeWith(function(input3) {
      var v = stripPrefix(str)(input3);
      if (v instanceof Just) {
        return new Right({
          value: str,
          consumed: str,
          remainder: v.value0
        });
      }
      ;
      return new Left("Expected " + show4(str));
    });
  };
  var $$char = function(c2) {
    return withErrorMessage(satisfy(function(v) {
      return v === c2;
    }))(show22(c2));
  };

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

  // output/Data.CodePoint.Unicode.Internal/index.js
  var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqInt);
  var NUMCAT_LU = /* @__PURE__ */ function() {
    function NUMCAT_LU2() {
    }
    ;
    NUMCAT_LU2.value = new NUMCAT_LU2();
    return NUMCAT_LU2;
  }();
  var NUMCAT_LL = /* @__PURE__ */ function() {
    function NUMCAT_LL2() {
    }
    ;
    NUMCAT_LL2.value = new NUMCAT_LL2();
    return NUMCAT_LL2;
  }();
  var NUMCAT_LT = /* @__PURE__ */ function() {
    function NUMCAT_LT2() {
    }
    ;
    NUMCAT_LT2.value = new NUMCAT_LT2();
    return NUMCAT_LT2;
  }();
  var NUMCAT_LM = /* @__PURE__ */ function() {
    function NUMCAT_LM2() {
    }
    ;
    NUMCAT_LM2.value = new NUMCAT_LM2();
    return NUMCAT_LM2;
  }();
  var NUMCAT_LO = /* @__PURE__ */ function() {
    function NUMCAT_LO2() {
    }
    ;
    NUMCAT_LO2.value = new NUMCAT_LO2();
    return NUMCAT_LO2;
  }();
  var NUMCAT_MN = /* @__PURE__ */ function() {
    function NUMCAT_MN2() {
    }
    ;
    NUMCAT_MN2.value = new NUMCAT_MN2();
    return NUMCAT_MN2;
  }();
  var NUMCAT_MC = /* @__PURE__ */ function() {
    function NUMCAT_MC2() {
    }
    ;
    NUMCAT_MC2.value = new NUMCAT_MC2();
    return NUMCAT_MC2;
  }();
  var NUMCAT_ME = /* @__PURE__ */ function() {
    function NUMCAT_ME2() {
    }
    ;
    NUMCAT_ME2.value = new NUMCAT_ME2();
    return NUMCAT_ME2;
  }();
  var NUMCAT_ND = /* @__PURE__ */ function() {
    function NUMCAT_ND2() {
    }
    ;
    NUMCAT_ND2.value = new NUMCAT_ND2();
    return NUMCAT_ND2;
  }();
  var NUMCAT_NL = /* @__PURE__ */ function() {
    function NUMCAT_NL2() {
    }
    ;
    NUMCAT_NL2.value = new NUMCAT_NL2();
    return NUMCAT_NL2;
  }();
  var NUMCAT_NO = /* @__PURE__ */ function() {
    function NUMCAT_NO2() {
    }
    ;
    NUMCAT_NO2.value = new NUMCAT_NO2();
    return NUMCAT_NO2;
  }();
  var NUMCAT_PC = /* @__PURE__ */ function() {
    function NUMCAT_PC2() {
    }
    ;
    NUMCAT_PC2.value = new NUMCAT_PC2();
    return NUMCAT_PC2;
  }();
  var NUMCAT_PD = /* @__PURE__ */ function() {
    function NUMCAT_PD2() {
    }
    ;
    NUMCAT_PD2.value = new NUMCAT_PD2();
    return NUMCAT_PD2;
  }();
  var NUMCAT_PS = /* @__PURE__ */ function() {
    function NUMCAT_PS2() {
    }
    ;
    NUMCAT_PS2.value = new NUMCAT_PS2();
    return NUMCAT_PS2;
  }();
  var NUMCAT_PE = /* @__PURE__ */ function() {
    function NUMCAT_PE2() {
    }
    ;
    NUMCAT_PE2.value = new NUMCAT_PE2();
    return NUMCAT_PE2;
  }();
  var NUMCAT_PI = /* @__PURE__ */ function() {
    function NUMCAT_PI2() {
    }
    ;
    NUMCAT_PI2.value = new NUMCAT_PI2();
    return NUMCAT_PI2;
  }();
  var NUMCAT_PF = /* @__PURE__ */ function() {
    function NUMCAT_PF2() {
    }
    ;
    NUMCAT_PF2.value = new NUMCAT_PF2();
    return NUMCAT_PF2;
  }();
  var NUMCAT_PO = /* @__PURE__ */ function() {
    function NUMCAT_PO2() {
    }
    ;
    NUMCAT_PO2.value = new NUMCAT_PO2();
    return NUMCAT_PO2;
  }();
  var NUMCAT_SM = /* @__PURE__ */ function() {
    function NUMCAT_SM2() {
    }
    ;
    NUMCAT_SM2.value = new NUMCAT_SM2();
    return NUMCAT_SM2;
  }();
  var NUMCAT_SC = /* @__PURE__ */ function() {
    function NUMCAT_SC2() {
    }
    ;
    NUMCAT_SC2.value = new NUMCAT_SC2();
    return NUMCAT_SC2;
  }();
  var NUMCAT_SK = /* @__PURE__ */ function() {
    function NUMCAT_SK2() {
    }
    ;
    NUMCAT_SK2.value = new NUMCAT_SK2();
    return NUMCAT_SK2;
  }();
  var NUMCAT_SO = /* @__PURE__ */ function() {
    function NUMCAT_SO2() {
    }
    ;
    NUMCAT_SO2.value = new NUMCAT_SO2();
    return NUMCAT_SO2;
  }();
  var NUMCAT_ZS = /* @__PURE__ */ function() {
    function NUMCAT_ZS2() {
    }
    ;
    NUMCAT_ZS2.value = new NUMCAT_ZS2();
    return NUMCAT_ZS2;
  }();
  var NUMCAT_ZL = /* @__PURE__ */ function() {
    function NUMCAT_ZL2() {
    }
    ;
    NUMCAT_ZL2.value = new NUMCAT_ZL2();
    return NUMCAT_ZL2;
  }();
  var NUMCAT_ZP = /* @__PURE__ */ function() {
    function NUMCAT_ZP2() {
    }
    ;
    NUMCAT_ZP2.value = new NUMCAT_ZP2();
    return NUMCAT_ZP2;
  }();
  var NUMCAT_CC = /* @__PURE__ */ function() {
    function NUMCAT_CC2() {
    }
    ;
    NUMCAT_CC2.value = new NUMCAT_CC2();
    return NUMCAT_CC2;
  }();
  var NUMCAT_CF = /* @__PURE__ */ function() {
    function NUMCAT_CF2() {
    }
    ;
    NUMCAT_CF2.value = new NUMCAT_CF2();
    return NUMCAT_CF2;
  }();
  var NUMCAT_CS = /* @__PURE__ */ function() {
    function NUMCAT_CS2() {
    }
    ;
    NUMCAT_CS2.value = new NUMCAT_CS2();
    return NUMCAT_CS2;
  }();
  var NUMCAT_CO = /* @__PURE__ */ function() {
    function NUMCAT_CO2() {
    }
    ;
    NUMCAT_CO2.value = new NUMCAT_CO2();
    return NUMCAT_CO2;
  }();
  var NUMCAT_CN = /* @__PURE__ */ function() {
    function NUMCAT_CN2() {
    }
    ;
    NUMCAT_CN2.value = new NUMCAT_CN2();
    return NUMCAT_CN2;
  }();
  var numSpaceBlocks = 7;
  var numLat1Blocks = 63;
  var numConvBlocks = 1332;
  var numBlocks = 3396;
  var gencatZS = 2;
  var rule1 = /* @__PURE__ */ function() {
    return {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var spacechars = [{
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }];
  var gencatZP = 67108864;
  var rule162 = /* @__PURE__ */ function() {
    return {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZL = 33554432;
  var rule161 = /* @__PURE__ */ function() {
    return {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSO = 8192;
  var rule13 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule170 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
    };
  }();
  var rule171 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
    };
  }();
  var gencatSM = 64;
  var rule6 = /* @__PURE__ */ function() {
    return {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSK = 1024;
  var rule10 = /* @__PURE__ */ function() {
    return {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSC = 8;
  var rule3 = /* @__PURE__ */ function() {
    return {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPS = 16;
  var rule4 = /* @__PURE__ */ function() {
    return {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPO = 4;
  var rule2 = /* @__PURE__ */ function() {
    return {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPI = 32768;
  var rule15 = /* @__PURE__ */ function() {
    return {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPF = 262144;
  var rule19 = /* @__PURE__ */ function() {
    return {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPE = 32;
  var rule5 = /* @__PURE__ */ function() {
    return {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPD = 128;
  var rule7 = /* @__PURE__ */ function() {
    return {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPC = 2048;
  var rule11 = /* @__PURE__ */ function() {
    return {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNO = 131072;
  var rule17 = /* @__PURE__ */ function() {
    return {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNL = 16777216;
  var rule128 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule168 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
    };
  }();
  var rule169 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
    };
  }();
  var gencatND = 256;
  var rule8 = /* @__PURE__ */ function() {
    return {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMN = 2097152;
  var rule92 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule93 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
    };
  }();
  var gencatME = 4194304;
  var rule119 = /* @__PURE__ */ function() {
    return {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMC = 8388608;
  var rule124 = /* @__PURE__ */ function() {
    return {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLU = 512;
  var nullrule = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule104 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
    };
  }();
  var rule107 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule115 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
    };
  }();
  var rule117 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
    };
  }();
  var rule118 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
    };
  }();
  var rule120 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
    };
  }();
  var rule122 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
    };
  }();
  var rule125 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
    };
  }();
  var rule127 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38864,
      titledist: 0
    };
  }();
  var rule137 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3008 | 0,
      titledist: 0
    };
  }();
  var rule142 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
    };
  }();
  var rule144 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule153 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
    };
  }();
  var rule156 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
    };
  }();
  var rule157 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
    };
  }();
  var rule158 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
    };
  }();
  var rule159 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
    };
  }();
  var rule160 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
    };
  }();
  var rule163 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
    };
  }();
  var rule164 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
    };
  }();
  var rule165 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
    };
  }();
  var rule166 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
    };
  }();
  var rule172 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
    };
  }();
  var rule173 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
    };
  }();
  var rule174 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
    };
  }();
  var rule177 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
    };
  }();
  var rule178 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
    };
  }();
  var rule179 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
    };
  }();
  var rule180 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
    };
  }();
  var rule181 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
    };
  }();
  var rule183 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
    };
  }();
  var rule184 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
    };
  }();
  var rule186 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42308 | 0,
      titledist: 0
    };
  }();
  var rule187 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42319 | 0,
      titledist: 0
    };
  }();
  var rule188 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42315 | 0,
      titledist: 0
    };
  }();
  var rule189 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42305 | 0,
      titledist: 0
    };
  }();
  var rule190 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42258 | 0,
      titledist: 0
    };
  }();
  var rule191 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42282 | 0,
      titledist: 0
    };
  }();
  var rule192 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42261 | 0,
      titledist: 0
    };
  }();
  var rule193 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 928,
      titledist: 0
    };
  }();
  var rule194 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -48 | 0,
      titledist: 0
    };
  }();
  var rule195 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42307 | 0,
      titledist: 0
    };
  }();
  var rule196 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35384 | 0,
      titledist: 0
    };
  }();
  var rule201 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
    };
  }();
  var rule203 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 34,
      titledist: 0
    };
  }();
  var rule22 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var rule24 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
    };
  }();
  var rule26 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
    };
  }();
  var rule29 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
    };
  }();
  var rule30 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
    };
  }();
  var rule31 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
    };
  }();
  var rule32 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
    };
  }();
  var rule33 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
    };
  }();
  var rule34 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
    };
  }();
  var rule35 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
    };
  }();
  var rule37 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
    };
  }();
  var rule38 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
    };
  }();
  var rule40 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
    };
  }();
  var rule42 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
    };
  }();
  var rule43 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
    };
  }();
  var rule44 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
    };
  }();
  var rule45 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
    };
  }();
  var rule47 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
    };
  }();
  var rule51 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
    };
  }();
  var rule52 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
    };
  }();
  var rule53 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
    };
  }();
  var rule54 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
    };
  }();
  var rule55 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
    };
  }();
  var rule56 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
    };
  }();
  var rule58 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
    };
  }();
  var rule59 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
    };
  }();
  var rule60 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
    };
  }();
  var rule9 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
    };
  }();
  var rule94 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 116,
      titledist: 0
    };
  }();
  var rule95 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
    };
  }();
  var rule96 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
    };
  }();
  var rule97 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
    };
  }();
  var rule98 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
    };
  }();
  var gencatLT = 524288;
  var rule151 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule154 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
    };
  }();
  var rule48 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var gencatLO = 16384;
  var rule14 = /* @__PURE__ */ function() {
    return {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLM = 1048576;
  var rule91 = /* @__PURE__ */ function() {
    return {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLL = 4096;
  var rule100 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
    };
  }();
  var rule101 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
    };
  }();
  var rule102 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
    };
  }();
  var rule103 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
    };
  }();
  var rule105 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
    };
  }();
  var rule106 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
    };
  }();
  var rule108 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
    };
  }();
  var rule109 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
    };
  }();
  var rule110 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
    };
  }();
  var rule111 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
    };
  }();
  var rule112 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
    };
  }();
  var rule113 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
    };
  }();
  var rule114 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -116 | 0,
      lowdist: 0,
      titledist: -116 | 0
    };
  }();
  var rule116 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
    };
  }();
  var rule12 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
    };
  }();
  var rule121 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
    };
  }();
  var rule123 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
    };
  }();
  var rule126 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3008,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule129 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6254 | 0,
      lowdist: 0,
      titledist: -6254 | 0
    };
  }();
  var rule130 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6253 | 0,
      lowdist: 0,
      titledist: -6253 | 0
    };
  }();
  var rule131 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6244 | 0,
      lowdist: 0,
      titledist: -6244 | 0
    };
  }();
  var rule132 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6242 | 0,
      lowdist: 0,
      titledist: -6242 | 0
    };
  }();
  var rule133 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6243 | 0,
      lowdist: 0,
      titledist: -6243 | 0
    };
  }();
  var rule134 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6236 | 0,
      lowdist: 0,
      titledist: -6236 | 0
    };
  }();
  var rule135 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6181 | 0,
      lowdist: 0,
      titledist: -6181 | 0
    };
  }();
  var rule136 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35266,
      lowdist: 0,
      titledist: 35266
    };
  }();
  var rule138 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
    };
  }();
  var rule139 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
    };
  }();
  var rule140 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35384,
      lowdist: 0,
      titledist: 35384
    };
  }();
  var rule141 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
    };
  }();
  var rule143 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
    };
  }();
  var rule145 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
    };
  }();
  var rule146 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
    };
  }();
  var rule147 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
    };
  }();
  var rule148 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
    };
  }();
  var rule149 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
    };
  }();
  var rule150 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
    };
  }();
  var rule152 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
    };
  }();
  var rule155 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
    };
  }();
  var rule167 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
    };
  }();
  var rule175 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
    };
  }();
  var rule176 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
    };
  }();
  var rule18 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
    };
  }();
  var rule182 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
    };
  }();
  var rule185 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 48,
      lowdist: 0,
      titledist: 48
    };
  }();
  var rule197 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -928 | 0,
      lowdist: 0,
      titledist: -928 | 0
    };
  }();
  var rule198 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38864 | 0,
      lowdist: 0,
      titledist: -38864 | 0
    };
  }();
  var rule20 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule202 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
    };
  }();
  var rule204 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -34 | 0,
      lowdist: 0,
      titledist: -34 | 0
    };
  }();
  var rule21 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
    };
  }();
  var rule23 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule25 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
    };
  }();
  var rule27 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
    };
  }();
  var rule28 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
    };
  }();
  var rule36 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
    };
  }();
  var rule39 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
    };
  }();
  var rule41 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
    };
  }();
  var rule46 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
    };
  }();
  var rule49 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule50 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
    };
  }();
  var rule57 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
    };
  }();
  var rule61 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
    };
  }();
  var rule62 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
    };
  }();
  var rule63 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
    };
  }();
  var rule64 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
    };
  }();
  var rule65 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
    };
  }();
  var rule66 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
    };
  }();
  var rule67 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
    };
  }();
  var rule68 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
    };
  }();
  var rule69 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42319,
      lowdist: 0,
      titledist: 42319
    };
  }();
  var rule70 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42315,
      lowdist: 0,
      titledist: 42315
    };
  }();
  var rule71 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
    };
  }();
  var rule72 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
    };
  }();
  var rule73 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42308,
      lowdist: 0,
      titledist: 42308
    };
  }();
  var rule74 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
    };
  }();
  var rule75 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
    };
  }();
  var rule76 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
    };
  }();
  var rule77 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42305,
      lowdist: 0,
      titledist: 42305
    };
  }();
  var rule78 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
    };
  }();
  var rule79 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
    };
  }();
  var rule80 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
    };
  }();
  var rule81 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
    };
  }();
  var rule82 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
    };
  }();
  var rule83 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42307,
      lowdist: 0,
      titledist: 42307
    };
  }();
  var rule84 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42282,
      lowdist: 0,
      titledist: 42282
    };
  }();
  var rule85 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
    };
  }();
  var rule86 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
    };
  }();
  var rule87 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
    };
  }();
  var rule88 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
    };
  }();
  var rule89 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42261,
      lowdist: 0,
      titledist: 42261
    };
  }();
  var rule90 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42258,
      lowdist: 0,
      titledist: 42258
    };
  }();
  var rule99 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
    };
  }();
  var gencatCS = 134217728;
  var rule199 = /* @__PURE__ */ function() {
    return {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCO = 268435456;
  var rule200 = /* @__PURE__ */ function() {
    return {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCF = 65536;
  var rule16 = /* @__PURE__ */ function() {
    return {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCC = 1;
  var rule0 = /* @__PURE__ */ function() {
    return {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var convchars = [{
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }];
  var bsearch = function(a3) {
    return function(array) {
      return function(size3) {
        return function(compare4) {
          var go = function($copy_i) {
            return function($copy_k) {
              var $tco_var_i = $copy_i;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(i, k2) {
                if (i > k2 || i >= length(array)) {
                  $tco_done = true;
                  return Nothing.value;
                }
                ;
                if (otherwise) {
                  var j = floor2(toNumber(i + k2 | 0) / 2);
                  var b2 = unsafeIndex2(array)(j);
                  var v = compare4(a3)(b2);
                  if (v instanceof EQ) {
                    $tco_done = true;
                    return new Just(b2);
                  }
                  ;
                  if (v instanceof GT) {
                    $tco_var_i = j + 1 | 0;
                    $copy_k = k2;
                    return;
                  }
                  ;
                  $tco_var_i = i;
                  $copy_k = j - 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i.constructor.name, k2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_k);
              }
              ;
              return $tco_result;
            };
          };
          return go(0)(size3);
        };
      };
    };
  };
  var blkCmp = function(v) {
    return function(v1) {
      if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
        return EQ.value;
      }
      ;
      if (v.start > v1.start) {
        return GT.value;
      }
      ;
      if (otherwise) {
        return LT.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getRule = function(blocks) {
    return function(unichar) {
      return function(size3) {
        var key2 = {
          start: unichar,
          length: 1,
          convRule: nullrule
        };
        var maybeCharBlock = bsearch(key2)(blocks)(size3)(blkCmp);
        if (maybeCharBlock instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (maybeCharBlock instanceof Just) {
          return new Just(maybeCharBlock.value0.convRule);
        }
        ;
        throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
      };
    };
  };
  var caseConv = function(f) {
    return function($$char2) {
      var maybeConversionRule = getRule(convchars)($$char2)(numConvBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return $$char2;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return $$char2 + f(maybeConversionRule.value0) | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5727, column 5 - line 5729, column 53): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uTowlower = /* @__PURE__ */ caseConv(function(v) {
    return v.lowdist;
  });
  var uTowupper = /* @__PURE__ */ caseConv(function(v) {
    return v.updist;
  });
  var checkAttrS = function(categories) {
    return function($$char2) {
      var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
  var allchars = [{
    start: 0,
    length: 32,
    convRule: rule0
  }, {
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 33,
    length: 3,
    convRule: rule2
  }, {
    start: 36,
    length: 1,
    convRule: rule3
  }, {
    start: 37,
    length: 3,
    convRule: rule2
  }, {
    start: 40,
    length: 1,
    convRule: rule4
  }, {
    start: 41,
    length: 1,
    convRule: rule5
  }, {
    start: 42,
    length: 1,
    convRule: rule2
  }, {
    start: 43,
    length: 1,
    convRule: rule6
  }, {
    start: 44,
    length: 1,
    convRule: rule2
  }, {
    start: 45,
    length: 1,
    convRule: rule7
  }, {
    start: 46,
    length: 2,
    convRule: rule2
  }, {
    start: 48,
    length: 10,
    convRule: rule8
  }, {
    start: 58,
    length: 2,
    convRule: rule2
  }, {
    start: 60,
    length: 3,
    convRule: rule6
  }, {
    start: 63,
    length: 2,
    convRule: rule2
  }, {
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 91,
    length: 1,
    convRule: rule4
  }, {
    start: 92,
    length: 1,
    convRule: rule2
  }, {
    start: 93,
    length: 1,
    convRule: rule5
  }, {
    start: 94,
    length: 1,
    convRule: rule10
  }, {
    start: 95,
    length: 1,
    convRule: rule11
  }, {
    start: 96,
    length: 1,
    convRule: rule10
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 123,
    length: 1,
    convRule: rule4
  }, {
    start: 124,
    length: 1,
    convRule: rule6
  }, {
    start: 125,
    length: 1,
    convRule: rule5
  }, {
    start: 126,
    length: 1,
    convRule: rule6
  }, {
    start: 127,
    length: 33,
    convRule: rule0
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 161,
    length: 1,
    convRule: rule2
  }, {
    start: 162,
    length: 4,
    convRule: rule3
  }, {
    start: 166,
    length: 1,
    convRule: rule13
  }, {
    start: 167,
    length: 1,
    convRule: rule2
  }, {
    start: 168,
    length: 1,
    convRule: rule10
  }, {
    start: 169,
    length: 1,
    convRule: rule13
  }, {
    start: 170,
    length: 1,
    convRule: rule14
  }, {
    start: 171,
    length: 1,
    convRule: rule15
  }, {
    start: 172,
    length: 1,
    convRule: rule6
  }, {
    start: 173,
    length: 1,
    convRule: rule16
  }, {
    start: 174,
    length: 1,
    convRule: rule13
  }, {
    start: 175,
    length: 1,
    convRule: rule10
  }, {
    start: 176,
    length: 1,
    convRule: rule13
  }, {
    start: 177,
    length: 1,
    convRule: rule6
  }, {
    start: 178,
    length: 2,
    convRule: rule17
  }, {
    start: 180,
    length: 1,
    convRule: rule10
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 182,
    length: 2,
    convRule: rule2
  }, {
    start: 184,
    length: 1,
    convRule: rule10
  }, {
    start: 185,
    length: 1,
    convRule: rule17
  }, {
    start: 186,
    length: 1,
    convRule: rule14
  }, {
    start: 187,
    length: 1,
    convRule: rule19
  }, {
    start: 188,
    length: 3,
    convRule: rule17
  }, {
    start: 191,
    length: 1,
    convRule: rule2
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 215,
    length: 1,
    convRule: rule6
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 223,
    length: 1,
    convRule: rule20
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 247,
    length: 1,
    convRule: rule6
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 312,
    length: 1,
    convRule: rule20
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 329,
    length: 1,
    convRule: rule20
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 397,
    length: 1,
    convRule: rule20
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 411,
    length: 1,
    convRule: rule20
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 426,
    length: 2,
    convRule: rule20
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 442,
    length: 1,
    convRule: rule20
  }, {
    start: 443,
    length: 1,
    convRule: rule14
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 446,
    length: 1,
    convRule: rule20
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 448,
    length: 4,
    convRule: rule14
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 496,
    length: 1,
    convRule: rule20
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 545,
    length: 1,
    convRule: rule20
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 564,
    length: 6,
    convRule: rule20
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 597,
    length: 1,
    convRule: rule20
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 600,
    length: 1,
    convRule: rule20
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 602,
    length: 1,
    convRule: rule20
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 605,
    length: 3,
    convRule: rule20
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 610,
    length: 1,
    convRule: rule20
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 612,
    length: 1,
    convRule: rule20
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 615,
    length: 1,
    convRule: rule20
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 621,
    length: 2,
    convRule: rule20
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 624,
    length: 1,
    convRule: rule20
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 627,
    length: 2,
    convRule: rule20
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 630,
    length: 7,
    convRule: rule20
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 638,
    length: 2,
    convRule: rule20
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 641,
    length: 1,
    convRule: rule20
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 644,
    length: 3,
    convRule: rule20
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 653,
    length: 5,
    convRule: rule20
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 659,
    length: 1,
    convRule: rule20
  }, {
    start: 660,
    length: 1,
    convRule: rule14
  }, {
    start: 661,
    length: 8,
    convRule: rule20
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 671,
    length: 17,
    convRule: rule20
  }, {
    start: 688,
    length: 18,
    convRule: rule91
  }, {
    start: 706,
    length: 4,
    convRule: rule10
  }, {
    start: 710,
    length: 12,
    convRule: rule91
  }, {
    start: 722,
    length: 14,
    convRule: rule10
  }, {
    start: 736,
    length: 5,
    convRule: rule91
  }, {
    start: 741,
    length: 7,
    convRule: rule10
  }, {
    start: 748,
    length: 1,
    convRule: rule91
  }, {
    start: 749,
    length: 1,
    convRule: rule10
  }, {
    start: 750,
    length: 1,
    convRule: rule91
  }, {
    start: 751,
    length: 17,
    convRule: rule10
  }, {
    start: 768,
    length: 69,
    convRule: rule92
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 838,
    length: 42,
    convRule: rule92
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 884,
    length: 1,
    convRule: rule91
  }, {
    start: 885,
    length: 1,
    convRule: rule10
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 890,
    length: 1,
    convRule: rule91
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 894,
    length: 1,
    convRule: rule2
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 900,
    length: 2,
    convRule: rule10
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 903,
    length: 1,
    convRule: rule2
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 912,
    length: 1,
    convRule: rule20
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 944,
    length: 1,
    convRule: rule20
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 978,
    length: 3,
    convRule: rule107
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1014,
    length: 1,
    convRule: rule6
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1020,
    length: 1,
    convRule: rule20
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1154,
    length: 1,
    convRule: rule13
  }, {
    start: 1155,
    length: 5,
    convRule: rule92
  }, {
    start: 1160,
    length: 2,
    convRule: rule119
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1369,
    length: 1,
    convRule: rule91
  }, {
    start: 1370,
    length: 6,
    convRule: rule2
  }, {
    start: 1376,
    length: 1,
    convRule: rule20
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 1415,
    length: 2,
    convRule: rule20
  }, {
    start: 1417,
    length: 1,
    convRule: rule2
  }, {
    start: 1418,
    length: 1,
    convRule: rule7
  }, {
    start: 1421,
    length: 2,
    convRule: rule13
  }, {
    start: 1423,
    length: 1,
    convRule: rule3
  }, {
    start: 1425,
    length: 45,
    convRule: rule92
  }, {
    start: 1470,
    length: 1,
    convRule: rule7
  }, {
    start: 1471,
    length: 1,
    convRule: rule92
  }, {
    start: 1472,
    length: 1,
    convRule: rule2
  }, {
    start: 1473,
    length: 2,
    convRule: rule92
  }, {
    start: 1475,
    length: 1,
    convRule: rule2
  }, {
    start: 1476,
    length: 2,
    convRule: rule92
  }, {
    start: 1478,
    length: 1,
    convRule: rule2
  }, {
    start: 1479,
    length: 1,
    convRule: rule92
  }, {
    start: 1488,
    length: 27,
    convRule: rule14
  }, {
    start: 1519,
    length: 4,
    convRule: rule14
  }, {
    start: 1523,
    length: 2,
    convRule: rule2
  }, {
    start: 1536,
    length: 6,
    convRule: rule16
  }, {
    start: 1542,
    length: 3,
    convRule: rule6
  }, {
    start: 1545,
    length: 2,
    convRule: rule2
  }, {
    start: 1547,
    length: 1,
    convRule: rule3
  }, {
    start: 1548,
    length: 2,
    convRule: rule2
  }, {
    start: 1550,
    length: 2,
    convRule: rule13
  }, {
    start: 1552,
    length: 11,
    convRule: rule92
  }, {
    start: 1563,
    length: 1,
    convRule: rule2
  }, {
    start: 1564,
    length: 1,
    convRule: rule16
  }, {
    start: 1566,
    length: 2,
    convRule: rule2
  }, {
    start: 1568,
    length: 32,
    convRule: rule14
  }, {
    start: 1600,
    length: 1,
    convRule: rule91
  }, {
    start: 1601,
    length: 10,
    convRule: rule14
  }, {
    start: 1611,
    length: 21,
    convRule: rule92
  }, {
    start: 1632,
    length: 10,
    convRule: rule8
  }, {
    start: 1642,
    length: 4,
    convRule: rule2
  }, {
    start: 1646,
    length: 2,
    convRule: rule14
  }, {
    start: 1648,
    length: 1,
    convRule: rule92
  }, {
    start: 1649,
    length: 99,
    convRule: rule14
  }, {
    start: 1748,
    length: 1,
    convRule: rule2
  }, {
    start: 1749,
    length: 1,
    convRule: rule14
  }, {
    start: 1750,
    length: 7,
    convRule: rule92
  }, {
    start: 1757,
    length: 1,
    convRule: rule16
  }, {
    start: 1758,
    length: 1,
    convRule: rule13
  }, {
    start: 1759,
    length: 6,
    convRule: rule92
  }, {
    start: 1765,
    length: 2,
    convRule: rule91
  }, {
    start: 1767,
    length: 2,
    convRule: rule92
  }, {
    start: 1769,
    length: 1,
    convRule: rule13
  }, {
    start: 1770,
    length: 4,
    convRule: rule92
  }, {
    start: 1774,
    length: 2,
    convRule: rule14
  }, {
    start: 1776,
    length: 10,
    convRule: rule8
  }, {
    start: 1786,
    length: 3,
    convRule: rule14
  }, {
    start: 1789,
    length: 2,
    convRule: rule13
  }, {
    start: 1791,
    length: 1,
    convRule: rule14
  }, {
    start: 1792,
    length: 14,
    convRule: rule2
  }, {
    start: 1807,
    length: 1,
    convRule: rule16
  }, {
    start: 1808,
    length: 1,
    convRule: rule14
  }, {
    start: 1809,
    length: 1,
    convRule: rule92
  }, {
    start: 1810,
    length: 30,
    convRule: rule14
  }, {
    start: 1840,
    length: 27,
    convRule: rule92
  }, {
    start: 1869,
    length: 89,
    convRule: rule14
  }, {
    start: 1958,
    length: 11,
    convRule: rule92
  }, {
    start: 1969,
    length: 1,
    convRule: rule14
  }, {
    start: 1984,
    length: 10,
    convRule: rule8
  }, {
    start: 1994,
    length: 33,
    convRule: rule14
  }, {
    start: 2027,
    length: 9,
    convRule: rule92
  }, {
    start: 2036,
    length: 2,
    convRule: rule91
  }, {
    start: 2038,
    length: 1,
    convRule: rule13
  }, {
    start: 2039,
    length: 3,
    convRule: rule2
  }, {
    start: 2042,
    length: 1,
    convRule: rule91
  }, {
    start: 2045,
    length: 1,
    convRule: rule92
  }, {
    start: 2046,
    length: 2,
    convRule: rule3
  }, {
    start: 2048,
    length: 22,
    convRule: rule14
  }, {
    start: 2070,
    length: 4,
    convRule: rule92
  }, {
    start: 2074,
    length: 1,
    convRule: rule91
  }, {
    start: 2075,
    length: 9,
    convRule: rule92
  }, {
    start: 2084,
    length: 1,
    convRule: rule91
  }, {
    start: 2085,
    length: 3,
    convRule: rule92
  }, {
    start: 2088,
    length: 1,
    convRule: rule91
  }, {
    start: 2089,
    length: 5,
    convRule: rule92
  }, {
    start: 2096,
    length: 15,
    convRule: rule2
  }, {
    start: 2112,
    length: 25,
    convRule: rule14
  }, {
    start: 2137,
    length: 3,
    convRule: rule92
  }, {
    start: 2142,
    length: 1,
    convRule: rule2
  }, {
    start: 2144,
    length: 11,
    convRule: rule14
  }, {
    start: 2208,
    length: 21,
    convRule: rule14
  }, {
    start: 2230,
    length: 18,
    convRule: rule14
  }, {
    start: 2259,
    length: 15,
    convRule: rule92
  }, {
    start: 2274,
    length: 1,
    convRule: rule16
  }, {
    start: 2275,
    length: 32,
    convRule: rule92
  }, {
    start: 2307,
    length: 1,
    convRule: rule124
  }, {
    start: 2308,
    length: 54,
    convRule: rule14
  }, {
    start: 2362,
    length: 1,
    convRule: rule92
  }, {
    start: 2363,
    length: 1,
    convRule: rule124
  }, {
    start: 2364,
    length: 1,
    convRule: rule92
  }, {
    start: 2365,
    length: 1,
    convRule: rule14
  }, {
    start: 2366,
    length: 3,
    convRule: rule124
  }, {
    start: 2369,
    length: 8,
    convRule: rule92
  }, {
    start: 2377,
    length: 4,
    convRule: rule124
  }, {
    start: 2381,
    length: 1,
    convRule: rule92
  }, {
    start: 2382,
    length: 2,
    convRule: rule124
  }, {
    start: 2384,
    length: 1,
    convRule: rule14
  }, {
    start: 2385,
    length: 7,
    convRule: rule92
  }, {
    start: 2392,
    length: 10,
    convRule: rule14
  }, {
    start: 2402,
    length: 2,
    convRule: rule92
  }, {
    start: 2404,
    length: 2,
    convRule: rule2
  }, {
    start: 2406,
    length: 10,
    convRule: rule8
  }, {
    start: 2416,
    length: 1,
    convRule: rule2
  }, {
    start: 2417,
    length: 1,
    convRule: rule91
  }, {
    start: 2418,
    length: 15,
    convRule: rule14
  }, {
    start: 2433,
    length: 1,
    convRule: rule92
  }, {
    start: 2434,
    length: 2,
    convRule: rule124
  }, {
    start: 2437,
    length: 8,
    convRule: rule14
  }, {
    start: 2447,
    length: 2,
    convRule: rule14
  }, {
    start: 2451,
    length: 22,
    convRule: rule14
  }, {
    start: 2474,
    length: 7,
    convRule: rule14
  }, {
    start: 2482,
    length: 1,
    convRule: rule14
  }, {
    start: 2486,
    length: 4,
    convRule: rule14
  }, {
    start: 2492,
    length: 1,
    convRule: rule92
  }, {
    start: 2493,
    length: 1,
    convRule: rule14
  }, {
    start: 2494,
    length: 3,
    convRule: rule124
  }, {
    start: 2497,
    length: 4,
    convRule: rule92
  }, {
    start: 2503,
    length: 2,
    convRule: rule124
  }, {
    start: 2507,
    length: 2,
    convRule: rule124
  }, {
    start: 2509,
    length: 1,
    convRule: rule92
  }, {
    start: 2510,
    length: 1,
    convRule: rule14
  }, {
    start: 2519,
    length: 1,
    convRule: rule124
  }, {
    start: 2524,
    length: 2,
    convRule: rule14
  }, {
    start: 2527,
    length: 3,
    convRule: rule14
  }, {
    start: 2530,
    length: 2,
    convRule: rule92
  }, {
    start: 2534,
    length: 10,
    convRule: rule8
  }, {
    start: 2544,
    length: 2,
    convRule: rule14
  }, {
    start: 2546,
    length: 2,
    convRule: rule3
  }, {
    start: 2548,
    length: 6,
    convRule: rule17
  }, {
    start: 2554,
    length: 1,
    convRule: rule13
  }, {
    start: 2555,
    length: 1,
    convRule: rule3
  }, {
    start: 2556,
    length: 1,
    convRule: rule14
  }, {
    start: 2557,
    length: 1,
    convRule: rule2
  }, {
    start: 2558,
    length: 1,
    convRule: rule92
  }, {
    start: 2561,
    length: 2,
    convRule: rule92
  }, {
    start: 2563,
    length: 1,
    convRule: rule124
  }, {
    start: 2565,
    length: 6,
    convRule: rule14
  }, {
    start: 2575,
    length: 2,
    convRule: rule14
  }, {
    start: 2579,
    length: 22,
    convRule: rule14
  }, {
    start: 2602,
    length: 7,
    convRule: rule14
  }, {
    start: 2610,
    length: 2,
    convRule: rule14
  }, {
    start: 2613,
    length: 2,
    convRule: rule14
  }, {
    start: 2616,
    length: 2,
    convRule: rule14
  }, {
    start: 2620,
    length: 1,
    convRule: rule92
  }, {
    start: 2622,
    length: 3,
    convRule: rule124
  }, {
    start: 2625,
    length: 2,
    convRule: rule92
  }, {
    start: 2631,
    length: 2,
    convRule: rule92
  }, {
    start: 2635,
    length: 3,
    convRule: rule92
  }, {
    start: 2641,
    length: 1,
    convRule: rule92
  }, {
    start: 2649,
    length: 4,
    convRule: rule14
  }, {
    start: 2654,
    length: 1,
    convRule: rule14
  }, {
    start: 2662,
    length: 10,
    convRule: rule8
  }, {
    start: 2672,
    length: 2,
    convRule: rule92
  }, {
    start: 2674,
    length: 3,
    convRule: rule14
  }, {
    start: 2677,
    length: 1,
    convRule: rule92
  }, {
    start: 2678,
    length: 1,
    convRule: rule2
  }, {
    start: 2689,
    length: 2,
    convRule: rule92
  }, {
    start: 2691,
    length: 1,
    convRule: rule124
  }, {
    start: 2693,
    length: 9,
    convRule: rule14
  }, {
    start: 2703,
    length: 3,
    convRule: rule14
  }, {
    start: 2707,
    length: 22,
    convRule: rule14
  }, {
    start: 2730,
    length: 7,
    convRule: rule14
  }, {
    start: 2738,
    length: 2,
    convRule: rule14
  }, {
    start: 2741,
    length: 5,
    convRule: rule14
  }, {
    start: 2748,
    length: 1,
    convRule: rule92
  }, {
    start: 2749,
    length: 1,
    convRule: rule14
  }, {
    start: 2750,
    length: 3,
    convRule: rule124
  }, {
    start: 2753,
    length: 5,
    convRule: rule92
  }, {
    start: 2759,
    length: 2,
    convRule: rule92
  }, {
    start: 2761,
    length: 1,
    convRule: rule124
  }, {
    start: 2763,
    length: 2,
    convRule: rule124
  }, {
    start: 2765,
    length: 1,
    convRule: rule92
  }, {
    start: 2768,
    length: 1,
    convRule: rule14
  }, {
    start: 2784,
    length: 2,
    convRule: rule14
  }, {
    start: 2786,
    length: 2,
    convRule: rule92
  }, {
    start: 2790,
    length: 10,
    convRule: rule8
  }, {
    start: 2800,
    length: 1,
    convRule: rule2
  }, {
    start: 2801,
    length: 1,
    convRule: rule3
  }, {
    start: 2809,
    length: 1,
    convRule: rule14
  }, {
    start: 2810,
    length: 6,
    convRule: rule92
  }, {
    start: 2817,
    length: 1,
    convRule: rule92
  }, {
    start: 2818,
    length: 2,
    convRule: rule124
  }, {
    start: 2821,
    length: 8,
    convRule: rule14
  }, {
    start: 2831,
    length: 2,
    convRule: rule14
  }, {
    start: 2835,
    length: 22,
    convRule: rule14
  }, {
    start: 2858,
    length: 7,
    convRule: rule14
  }, {
    start: 2866,
    length: 2,
    convRule: rule14
  }, {
    start: 2869,
    length: 5,
    convRule: rule14
  }, {
    start: 2876,
    length: 1,
    convRule: rule92
  }, {
    start: 2877,
    length: 1,
    convRule: rule14
  }, {
    start: 2878,
    length: 1,
    convRule: rule124
  }, {
    start: 2879,
    length: 1,
    convRule: rule92
  }, {
    start: 2880,
    length: 1,
    convRule: rule124
  }, {
    start: 2881,
    length: 4,
    convRule: rule92
  }, {
    start: 2887,
    length: 2,
    convRule: rule124
  }, {
    start: 2891,
    length: 2,
    convRule: rule124
  }, {
    start: 2893,
    length: 1,
    convRule: rule92
  }, {
    start: 2901,
    length: 2,
    convRule: rule92
  }, {
    start: 2903,
    length: 1,
    convRule: rule124
  }, {
    start: 2908,
    length: 2,
    convRule: rule14
  }, {
    start: 2911,
    length: 3,
    convRule: rule14
  }, {
    start: 2914,
    length: 2,
    convRule: rule92
  }, {
    start: 2918,
    length: 10,
    convRule: rule8
  }, {
    start: 2928,
    length: 1,
    convRule: rule13
  }, {
    start: 2929,
    length: 1,
    convRule: rule14
  }, {
    start: 2930,
    length: 6,
    convRule: rule17
  }, {
    start: 2946,
    length: 1,
    convRule: rule92
  }, {
    start: 2947,
    length: 1,
    convRule: rule14
  }, {
    start: 2949,
    length: 6,
    convRule: rule14
  }, {
    start: 2958,
    length: 3,
    convRule: rule14
  }, {
    start: 2962,
    length: 4,
    convRule: rule14
  }, {
    start: 2969,
    length: 2,
    convRule: rule14
  }, {
    start: 2972,
    length: 1,
    convRule: rule14
  }, {
    start: 2974,
    length: 2,
    convRule: rule14
  }, {
    start: 2979,
    length: 2,
    convRule: rule14
  }, {
    start: 2984,
    length: 3,
    convRule: rule14
  }, {
    start: 2990,
    length: 12,
    convRule: rule14
  }, {
    start: 3006,
    length: 2,
    convRule: rule124
  }, {
    start: 3008,
    length: 1,
    convRule: rule92
  }, {
    start: 3009,
    length: 2,
    convRule: rule124
  }, {
    start: 3014,
    length: 3,
    convRule: rule124
  }, {
    start: 3018,
    length: 3,
    convRule: rule124
  }, {
    start: 3021,
    length: 1,
    convRule: rule92
  }, {
    start: 3024,
    length: 1,
    convRule: rule14
  }, {
    start: 3031,
    length: 1,
    convRule: rule124
  }, {
    start: 3046,
    length: 10,
    convRule: rule8
  }, {
    start: 3056,
    length: 3,
    convRule: rule17
  }, {
    start: 3059,
    length: 6,
    convRule: rule13
  }, {
    start: 3065,
    length: 1,
    convRule: rule3
  }, {
    start: 3066,
    length: 1,
    convRule: rule13
  }, {
    start: 3072,
    length: 1,
    convRule: rule92
  }, {
    start: 3073,
    length: 3,
    convRule: rule124
  }, {
    start: 3076,
    length: 1,
    convRule: rule92
  }, {
    start: 3077,
    length: 8,
    convRule: rule14
  }, {
    start: 3086,
    length: 3,
    convRule: rule14
  }, {
    start: 3090,
    length: 23,
    convRule: rule14
  }, {
    start: 3114,
    length: 16,
    convRule: rule14
  }, {
    start: 3133,
    length: 1,
    convRule: rule14
  }, {
    start: 3134,
    length: 3,
    convRule: rule92
  }, {
    start: 3137,
    length: 4,
    convRule: rule124
  }, {
    start: 3142,
    length: 3,
    convRule: rule92
  }, {
    start: 3146,
    length: 4,
    convRule: rule92
  }, {
    start: 3157,
    length: 2,
    convRule: rule92
  }, {
    start: 3160,
    length: 3,
    convRule: rule14
  }, {
    start: 3168,
    length: 2,
    convRule: rule14
  }, {
    start: 3170,
    length: 2,
    convRule: rule92
  }, {
    start: 3174,
    length: 10,
    convRule: rule8
  }, {
    start: 3191,
    length: 1,
    convRule: rule2
  }, {
    start: 3192,
    length: 7,
    convRule: rule17
  }, {
    start: 3199,
    length: 1,
    convRule: rule13
  }, {
    start: 3200,
    length: 1,
    convRule: rule14
  }, {
    start: 3201,
    length: 1,
    convRule: rule92
  }, {
    start: 3202,
    length: 2,
    convRule: rule124
  }, {
    start: 3204,
    length: 1,
    convRule: rule2
  }, {
    start: 3205,
    length: 8,
    convRule: rule14
  }, {
    start: 3214,
    length: 3,
    convRule: rule14
  }, {
    start: 3218,
    length: 23,
    convRule: rule14
  }, {
    start: 3242,
    length: 10,
    convRule: rule14
  }, {
    start: 3253,
    length: 5,
    convRule: rule14
  }, {
    start: 3260,
    length: 1,
    convRule: rule92
  }, {
    start: 3261,
    length: 1,
    convRule: rule14
  }, {
    start: 3262,
    length: 1,
    convRule: rule124
  }, {
    start: 3263,
    length: 1,
    convRule: rule92
  }, {
    start: 3264,
    length: 5,
    convRule: rule124
  }, {
    start: 3270,
    length: 1,
    convRule: rule92
  }, {
    start: 3271,
    length: 2,
    convRule: rule124
  }, {
    start: 3274,
    length: 2,
    convRule: rule124
  }, {
    start: 3276,
    length: 2,
    convRule: rule92
  }, {
    start: 3285,
    length: 2,
    convRule: rule124
  }, {
    start: 3294,
    length: 1,
    convRule: rule14
  }, {
    start: 3296,
    length: 2,
    convRule: rule14
  }, {
    start: 3298,
    length: 2,
    convRule: rule92
  }, {
    start: 3302,
    length: 10,
    convRule: rule8
  }, {
    start: 3313,
    length: 2,
    convRule: rule14
  }, {
    start: 3328,
    length: 2,
    convRule: rule92
  }, {
    start: 3330,
    length: 2,
    convRule: rule124
  }, {
    start: 3332,
    length: 9,
    convRule: rule14
  }, {
    start: 3342,
    length: 3,
    convRule: rule14
  }, {
    start: 3346,
    length: 41,
    convRule: rule14
  }, {
    start: 3387,
    length: 2,
    convRule: rule92
  }, {
    start: 3389,
    length: 1,
    convRule: rule14
  }, {
    start: 3390,
    length: 3,
    convRule: rule124
  }, {
    start: 3393,
    length: 4,
    convRule: rule92
  }, {
    start: 3398,
    length: 3,
    convRule: rule124
  }, {
    start: 3402,
    length: 3,
    convRule: rule124
  }, {
    start: 3405,
    length: 1,
    convRule: rule92
  }, {
    start: 3406,
    length: 1,
    convRule: rule14
  }, {
    start: 3407,
    length: 1,
    convRule: rule13
  }, {
    start: 3412,
    length: 3,
    convRule: rule14
  }, {
    start: 3415,
    length: 1,
    convRule: rule124
  }, {
    start: 3416,
    length: 7,
    convRule: rule17
  }, {
    start: 3423,
    length: 3,
    convRule: rule14
  }, {
    start: 3426,
    length: 2,
    convRule: rule92
  }, {
    start: 3430,
    length: 10,
    convRule: rule8
  }, {
    start: 3440,
    length: 9,
    convRule: rule17
  }, {
    start: 3449,
    length: 1,
    convRule: rule13
  }, {
    start: 3450,
    length: 6,
    convRule: rule14
  }, {
    start: 3457,
    length: 1,
    convRule: rule92
  }, {
    start: 3458,
    length: 2,
    convRule: rule124
  }, {
    start: 3461,
    length: 18,
    convRule: rule14
  }, {
    start: 3482,
    length: 24,
    convRule: rule14
  }, {
    start: 3507,
    length: 9,
    convRule: rule14
  }, {
    start: 3517,
    length: 1,
    convRule: rule14
  }, {
    start: 3520,
    length: 7,
    convRule: rule14
  }, {
    start: 3530,
    length: 1,
    convRule: rule92
  }, {
    start: 3535,
    length: 3,
    convRule: rule124
  }, {
    start: 3538,
    length: 3,
    convRule: rule92
  }, {
    start: 3542,
    length: 1,
    convRule: rule92
  }, {
    start: 3544,
    length: 8,
    convRule: rule124
  }, {
    start: 3558,
    length: 10,
    convRule: rule8
  }, {
    start: 3570,
    length: 2,
    convRule: rule124
  }, {
    start: 3572,
    length: 1,
    convRule: rule2
  }, {
    start: 3585,
    length: 48,
    convRule: rule14
  }, {
    start: 3633,
    length: 1,
    convRule: rule92
  }, {
    start: 3634,
    length: 2,
    convRule: rule14
  }, {
    start: 3636,
    length: 7,
    convRule: rule92
  }, {
    start: 3647,
    length: 1,
    convRule: rule3
  }, {
    start: 3648,
    length: 6,
    convRule: rule14
  }, {
    start: 3654,
    length: 1,
    convRule: rule91
  }, {
    start: 3655,
    length: 8,
    convRule: rule92
  }, {
    start: 3663,
    length: 1,
    convRule: rule2
  }, {
    start: 3664,
    length: 10,
    convRule: rule8
  }, {
    start: 3674,
    length: 2,
    convRule: rule2
  }, {
    start: 3713,
    length: 2,
    convRule: rule14
  }, {
    start: 3716,
    length: 1,
    convRule: rule14
  }, {
    start: 3718,
    length: 5,
    convRule: rule14
  }, {
    start: 3724,
    length: 24,
    convRule: rule14
  }, {
    start: 3749,
    length: 1,
    convRule: rule14
  }, {
    start: 3751,
    length: 10,
    convRule: rule14
  }, {
    start: 3761,
    length: 1,
    convRule: rule92
  }, {
    start: 3762,
    length: 2,
    convRule: rule14
  }, {
    start: 3764,
    length: 9,
    convRule: rule92
  }, {
    start: 3773,
    length: 1,
    convRule: rule14
  }, {
    start: 3776,
    length: 5,
    convRule: rule14
  }, {
    start: 3782,
    length: 1,
    convRule: rule91
  }, {
    start: 3784,
    length: 6,
    convRule: rule92
  }, {
    start: 3792,
    length: 10,
    convRule: rule8
  }, {
    start: 3804,
    length: 4,
    convRule: rule14
  }, {
    start: 3840,
    length: 1,
    convRule: rule14
  }, {
    start: 3841,
    length: 3,
    convRule: rule13
  }, {
    start: 3844,
    length: 15,
    convRule: rule2
  }, {
    start: 3859,
    length: 1,
    convRule: rule13
  }, {
    start: 3860,
    length: 1,
    convRule: rule2
  }, {
    start: 3861,
    length: 3,
    convRule: rule13
  }, {
    start: 3864,
    length: 2,
    convRule: rule92
  }, {
    start: 3866,
    length: 6,
    convRule: rule13
  }, {
    start: 3872,
    length: 10,
    convRule: rule8
  }, {
    start: 3882,
    length: 10,
    convRule: rule17
  }, {
    start: 3892,
    length: 1,
    convRule: rule13
  }, {
    start: 3893,
    length: 1,
    convRule: rule92
  }, {
    start: 3894,
    length: 1,
    convRule: rule13
  }, {
    start: 3895,
    length: 1,
    convRule: rule92
  }, {
    start: 3896,
    length: 1,
    convRule: rule13
  }, {
    start: 3897,
    length: 1,
    convRule: rule92
  }, {
    start: 3898,
    length: 1,
    convRule: rule4
  }, {
    start: 3899,
    length: 1,
    convRule: rule5
  }, {
    start: 3900,
    length: 1,
    convRule: rule4
  }, {
    start: 3901,
    length: 1,
    convRule: rule5
  }, {
    start: 3902,
    length: 2,
    convRule: rule124
  }, {
    start: 3904,
    length: 8,
    convRule: rule14
  }, {
    start: 3913,
    length: 36,
    convRule: rule14
  }, {
    start: 3953,
    length: 14,
    convRule: rule92
  }, {
    start: 3967,
    length: 1,
    convRule: rule124
  }, {
    start: 3968,
    length: 5,
    convRule: rule92
  }, {
    start: 3973,
    length: 1,
    convRule: rule2
  }, {
    start: 3974,
    length: 2,
    convRule: rule92
  }, {
    start: 3976,
    length: 5,
    convRule: rule14
  }, {
    start: 3981,
    length: 11,
    convRule: rule92
  }, {
    start: 3993,
    length: 36,
    convRule: rule92
  }, {
    start: 4030,
    length: 8,
    convRule: rule13
  }, {
    start: 4038,
    length: 1,
    convRule: rule92
  }, {
    start: 4039,
    length: 6,
    convRule: rule13
  }, {
    start: 4046,
    length: 2,
    convRule: rule13
  }, {
    start: 4048,
    length: 5,
    convRule: rule2
  }, {
    start: 4053,
    length: 4,
    convRule: rule13
  }, {
    start: 4057,
    length: 2,
    convRule: rule2
  }, {
    start: 4096,
    length: 43,
    convRule: rule14
  }, {
    start: 4139,
    length: 2,
    convRule: rule124
  }, {
    start: 4141,
    length: 4,
    convRule: rule92
  }, {
    start: 4145,
    length: 1,
    convRule: rule124
  }, {
    start: 4146,
    length: 6,
    convRule: rule92
  }, {
    start: 4152,
    length: 1,
    convRule: rule124
  }, {
    start: 4153,
    length: 2,
    convRule: rule92
  }, {
    start: 4155,
    length: 2,
    convRule: rule124
  }, {
    start: 4157,
    length: 2,
    convRule: rule92
  }, {
    start: 4159,
    length: 1,
    convRule: rule14
  }, {
    start: 4160,
    length: 10,
    convRule: rule8
  }, {
    start: 4170,
    length: 6,
    convRule: rule2
  }, {
    start: 4176,
    length: 6,
    convRule: rule14
  }, {
    start: 4182,
    length: 2,
    convRule: rule124
  }, {
    start: 4184,
    length: 2,
    convRule: rule92
  }, {
    start: 4186,
    length: 4,
    convRule: rule14
  }, {
    start: 4190,
    length: 3,
    convRule: rule92
  }, {
    start: 4193,
    length: 1,
    convRule: rule14
  }, {
    start: 4194,
    length: 3,
    convRule: rule124
  }, {
    start: 4197,
    length: 2,
    convRule: rule14
  }, {
    start: 4199,
    length: 7,
    convRule: rule124
  }, {
    start: 4206,
    length: 3,
    convRule: rule14
  }, {
    start: 4209,
    length: 4,
    convRule: rule92
  }, {
    start: 4213,
    length: 13,
    convRule: rule14
  }, {
    start: 4226,
    length: 1,
    convRule: rule92
  }, {
    start: 4227,
    length: 2,
    convRule: rule124
  }, {
    start: 4229,
    length: 2,
    convRule: rule92
  }, {
    start: 4231,
    length: 6,
    convRule: rule124
  }, {
    start: 4237,
    length: 1,
    convRule: rule92
  }, {
    start: 4238,
    length: 1,
    convRule: rule14
  }, {
    start: 4239,
    length: 1,
    convRule: rule124
  }, {
    start: 4240,
    length: 10,
    convRule: rule8
  }, {
    start: 4250,
    length: 3,
    convRule: rule124
  }, {
    start: 4253,
    length: 1,
    convRule: rule92
  }, {
    start: 4254,
    length: 2,
    convRule: rule13
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4347,
    length: 1,
    convRule: rule2
  }, {
    start: 4348,
    length: 1,
    convRule: rule91
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 4352,
    length: 329,
    convRule: rule14
  }, {
    start: 4682,
    length: 4,
    convRule: rule14
  }, {
    start: 4688,
    length: 7,
    convRule: rule14
  }, {
    start: 4696,
    length: 1,
    convRule: rule14
  }, {
    start: 4698,
    length: 4,
    convRule: rule14
  }, {
    start: 4704,
    length: 41,
    convRule: rule14
  }, {
    start: 4746,
    length: 4,
    convRule: rule14
  }, {
    start: 4752,
    length: 33,
    convRule: rule14
  }, {
    start: 4786,
    length: 4,
    convRule: rule14
  }, {
    start: 4792,
    length: 7,
    convRule: rule14
  }, {
    start: 4800,
    length: 1,
    convRule: rule14
  }, {
    start: 4802,
    length: 4,
    convRule: rule14
  }, {
    start: 4808,
    length: 15,
    convRule: rule14
  }, {
    start: 4824,
    length: 57,
    convRule: rule14
  }, {
    start: 4882,
    length: 4,
    convRule: rule14
  }, {
    start: 4888,
    length: 67,
    convRule: rule14
  }, {
    start: 4957,
    length: 3,
    convRule: rule92
  }, {
    start: 4960,
    length: 9,
    convRule: rule2
  }, {
    start: 4969,
    length: 20,
    convRule: rule17
  }, {
    start: 4992,
    length: 16,
    convRule: rule14
  }, {
    start: 5008,
    length: 10,
    convRule: rule13
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 5120,
    length: 1,
    convRule: rule7
  }, {
    start: 5121,
    length: 620,
    convRule: rule14
  }, {
    start: 5741,
    length: 1,
    convRule: rule13
  }, {
    start: 5742,
    length: 1,
    convRule: rule2
  }, {
    start: 5743,
    length: 17,
    convRule: rule14
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 5761,
    length: 26,
    convRule: rule14
  }, {
    start: 5787,
    length: 1,
    convRule: rule4
  }, {
    start: 5788,
    length: 1,
    convRule: rule5
  }, {
    start: 5792,
    length: 75,
    convRule: rule14
  }, {
    start: 5867,
    length: 3,
    convRule: rule2
  }, {
    start: 5870,
    length: 3,
    convRule: rule128
  }, {
    start: 5873,
    length: 8,
    convRule: rule14
  }, {
    start: 5888,
    length: 13,
    convRule: rule14
  }, {
    start: 5902,
    length: 4,
    convRule: rule14
  }, {
    start: 5906,
    length: 3,
    convRule: rule92
  }, {
    start: 5920,
    length: 18,
    convRule: rule14
  }, {
    start: 5938,
    length: 3,
    convRule: rule92
  }, {
    start: 5941,
    length: 2,
    convRule: rule2
  }, {
    start: 5952,
    length: 18,
    convRule: rule14
  }, {
    start: 5970,
    length: 2,
    convRule: rule92
  }, {
    start: 5984,
    length: 13,
    convRule: rule14
  }, {
    start: 5998,
    length: 3,
    convRule: rule14
  }, {
    start: 6002,
    length: 2,
    convRule: rule92
  }, {
    start: 6016,
    length: 52,
    convRule: rule14
  }, {
    start: 6068,
    length: 2,
    convRule: rule92
  }, {
    start: 6070,
    length: 1,
    convRule: rule124
  }, {
    start: 6071,
    length: 7,
    convRule: rule92
  }, {
    start: 6078,
    length: 8,
    convRule: rule124
  }, {
    start: 6086,
    length: 1,
    convRule: rule92
  }, {
    start: 6087,
    length: 2,
    convRule: rule124
  }, {
    start: 6089,
    length: 11,
    convRule: rule92
  }, {
    start: 6100,
    length: 3,
    convRule: rule2
  }, {
    start: 6103,
    length: 1,
    convRule: rule91
  }, {
    start: 6104,
    length: 3,
    convRule: rule2
  }, {
    start: 6107,
    length: 1,
    convRule: rule3
  }, {
    start: 6108,
    length: 1,
    convRule: rule14
  }, {
    start: 6109,
    length: 1,
    convRule: rule92
  }, {
    start: 6112,
    length: 10,
    convRule: rule8
  }, {
    start: 6128,
    length: 10,
    convRule: rule17
  }, {
    start: 6144,
    length: 6,
    convRule: rule2
  }, {
    start: 6150,
    length: 1,
    convRule: rule7
  }, {
    start: 6151,
    length: 4,
    convRule: rule2
  }, {
    start: 6155,
    length: 3,
    convRule: rule92
  }, {
    start: 6158,
    length: 1,
    convRule: rule16
  }, {
    start: 6160,
    length: 10,
    convRule: rule8
  }, {
    start: 6176,
    length: 35,
    convRule: rule14
  }, {
    start: 6211,
    length: 1,
    convRule: rule91
  }, {
    start: 6212,
    length: 53,
    convRule: rule14
  }, {
    start: 6272,
    length: 5,
    convRule: rule14
  }, {
    start: 6277,
    length: 2,
    convRule: rule92
  }, {
    start: 6279,
    length: 34,
    convRule: rule14
  }, {
    start: 6313,
    length: 1,
    convRule: rule92
  }, {
    start: 6314,
    length: 1,
    convRule: rule14
  }, {
    start: 6320,
    length: 70,
    convRule: rule14
  }, {
    start: 6400,
    length: 31,
    convRule: rule14
  }, {
    start: 6432,
    length: 3,
    convRule: rule92
  }, {
    start: 6435,
    length: 4,
    convRule: rule124
  }, {
    start: 6439,
    length: 2,
    convRule: rule92
  }, {
    start: 6441,
    length: 3,
    convRule: rule124
  }, {
    start: 6448,
    length: 2,
    convRule: rule124
  }, {
    start: 6450,
    length: 1,
    convRule: rule92
  }, {
    start: 6451,
    length: 6,
    convRule: rule124
  }, {
    start: 6457,
    length: 3,
    convRule: rule92
  }, {
    start: 6464,
    length: 1,
    convRule: rule13
  }, {
    start: 6468,
    length: 2,
    convRule: rule2
  }, {
    start: 6470,
    length: 10,
    convRule: rule8
  }, {
    start: 6480,
    length: 30,
    convRule: rule14
  }, {
    start: 6512,
    length: 5,
    convRule: rule14
  }, {
    start: 6528,
    length: 44,
    convRule: rule14
  }, {
    start: 6576,
    length: 26,
    convRule: rule14
  }, {
    start: 6608,
    length: 10,
    convRule: rule8
  }, {
    start: 6618,
    length: 1,
    convRule: rule17
  }, {
    start: 6622,
    length: 34,
    convRule: rule13
  }, {
    start: 6656,
    length: 23,
    convRule: rule14
  }, {
    start: 6679,
    length: 2,
    convRule: rule92
  }, {
    start: 6681,
    length: 2,
    convRule: rule124
  }, {
    start: 6683,
    length: 1,
    convRule: rule92
  }, {
    start: 6686,
    length: 2,
    convRule: rule2
  }, {
    start: 6688,
    length: 53,
    convRule: rule14
  }, {
    start: 6741,
    length: 1,
    convRule: rule124
  }, {
    start: 6742,
    length: 1,
    convRule: rule92
  }, {
    start: 6743,
    length: 1,
    convRule: rule124
  }, {
    start: 6744,
    length: 7,
    convRule: rule92
  }, {
    start: 6752,
    length: 1,
    convRule: rule92
  }, {
    start: 6753,
    length: 1,
    convRule: rule124
  }, {
    start: 6754,
    length: 1,
    convRule: rule92
  }, {
    start: 6755,
    length: 2,
    convRule: rule124
  }, {
    start: 6757,
    length: 8,
    convRule: rule92
  }, {
    start: 6765,
    length: 6,
    convRule: rule124
  }, {
    start: 6771,
    length: 10,
    convRule: rule92
  }, {
    start: 6783,
    length: 1,
    convRule: rule92
  }, {
    start: 6784,
    length: 10,
    convRule: rule8
  }, {
    start: 6800,
    length: 10,
    convRule: rule8
  }, {
    start: 6816,
    length: 7,
    convRule: rule2
  }, {
    start: 6823,
    length: 1,
    convRule: rule91
  }, {
    start: 6824,
    length: 6,
    convRule: rule2
  }, {
    start: 6832,
    length: 14,
    convRule: rule92
  }, {
    start: 6846,
    length: 1,
    convRule: rule119
  }, {
    start: 6847,
    length: 2,
    convRule: rule92
  }, {
    start: 6912,
    length: 4,
    convRule: rule92
  }, {
    start: 6916,
    length: 1,
    convRule: rule124
  }, {
    start: 6917,
    length: 47,
    convRule: rule14
  }, {
    start: 6964,
    length: 1,
    convRule: rule92
  }, {
    start: 6965,
    length: 1,
    convRule: rule124
  }, {
    start: 6966,
    length: 5,
    convRule: rule92
  }, {
    start: 6971,
    length: 1,
    convRule: rule124
  }, {
    start: 6972,
    length: 1,
    convRule: rule92
  }, {
    start: 6973,
    length: 5,
    convRule: rule124
  }, {
    start: 6978,
    length: 1,
    convRule: rule92
  }, {
    start: 6979,
    length: 2,
    convRule: rule124
  }, {
    start: 6981,
    length: 7,
    convRule: rule14
  }, {
    start: 6992,
    length: 10,
    convRule: rule8
  }, {
    start: 7002,
    length: 7,
    convRule: rule2
  }, {
    start: 7009,
    length: 10,
    convRule: rule13
  }, {
    start: 7019,
    length: 9,
    convRule: rule92
  }, {
    start: 7028,
    length: 9,
    convRule: rule13
  }, {
    start: 7040,
    length: 2,
    convRule: rule92
  }, {
    start: 7042,
    length: 1,
    convRule: rule124
  }, {
    start: 7043,
    length: 30,
    convRule: rule14
  }, {
    start: 7073,
    length: 1,
    convRule: rule124
  }, {
    start: 7074,
    length: 4,
    convRule: rule92
  }, {
    start: 7078,
    length: 2,
    convRule: rule124
  }, {
    start: 7080,
    length: 2,
    convRule: rule92
  }, {
    start: 7082,
    length: 1,
    convRule: rule124
  }, {
    start: 7083,
    length: 3,
    convRule: rule92
  }, {
    start: 7086,
    length: 2,
    convRule: rule14
  }, {
    start: 7088,
    length: 10,
    convRule: rule8
  }, {
    start: 7098,
    length: 44,
    convRule: rule14
  }, {
    start: 7142,
    length: 1,
    convRule: rule92
  }, {
    start: 7143,
    length: 1,
    convRule: rule124
  }, {
    start: 7144,
    length: 2,
    convRule: rule92
  }, {
    start: 7146,
    length: 3,
    convRule: rule124
  }, {
    start: 7149,
    length: 1,
    convRule: rule92
  }, {
    start: 7150,
    length: 1,
    convRule: rule124
  }, {
    start: 7151,
    length: 3,
    convRule: rule92
  }, {
    start: 7154,
    length: 2,
    convRule: rule124
  }, {
    start: 7164,
    length: 4,
    convRule: rule2
  }, {
    start: 7168,
    length: 36,
    convRule: rule14
  }, {
    start: 7204,
    length: 8,
    convRule: rule124
  }, {
    start: 7212,
    length: 8,
    convRule: rule92
  }, {
    start: 7220,
    length: 2,
    convRule: rule124
  }, {
    start: 7222,
    length: 2,
    convRule: rule92
  }, {
    start: 7227,
    length: 5,
    convRule: rule2
  }, {
    start: 7232,
    length: 10,
    convRule: rule8
  }, {
    start: 7245,
    length: 3,
    convRule: rule14
  }, {
    start: 7248,
    length: 10,
    convRule: rule8
  }, {
    start: 7258,
    length: 30,
    convRule: rule14
  }, {
    start: 7288,
    length: 6,
    convRule: rule91
  }, {
    start: 7294,
    length: 2,
    convRule: rule2
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7360,
    length: 8,
    convRule: rule2
  }, {
    start: 7376,
    length: 3,
    convRule: rule92
  }, {
    start: 7379,
    length: 1,
    convRule: rule2
  }, {
    start: 7380,
    length: 13,
    convRule: rule92
  }, {
    start: 7393,
    length: 1,
    convRule: rule124
  }, {
    start: 7394,
    length: 7,
    convRule: rule92
  }, {
    start: 7401,
    length: 4,
    convRule: rule14
  }, {
    start: 7405,
    length: 1,
    convRule: rule92
  }, {
    start: 7406,
    length: 6,
    convRule: rule14
  }, {
    start: 7412,
    length: 1,
    convRule: rule92
  }, {
    start: 7413,
    length: 2,
    convRule: rule14
  }, {
    start: 7415,
    length: 1,
    convRule: rule124
  }, {
    start: 7416,
    length: 2,
    convRule: rule92
  }, {
    start: 7418,
    length: 1,
    convRule: rule14
  }, {
    start: 7424,
    length: 44,
    convRule: rule20
  }, {
    start: 7468,
    length: 63,
    convRule: rule91
  }, {
    start: 7531,
    length: 13,
    convRule: rule20
  }, {
    start: 7544,
    length: 1,
    convRule: rule91
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7546,
    length: 3,
    convRule: rule20
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7550,
    length: 16,
    convRule: rule20
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7567,
    length: 12,
    convRule: rule20
  }, {
    start: 7579,
    length: 37,
    convRule: rule91
  }, {
    start: 7616,
    length: 58,
    convRule: rule92
  }, {
    start: 7675,
    length: 5,
    convRule: rule92
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7830,
    length: 5,
    convRule: rule20
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7836,
    length: 2,
    convRule: rule20
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7839,
    length: 1,
    convRule: rule20
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8016,
    length: 1,
    convRule: rule20
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8018,
    length: 1,
    convRule: rule20
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8020,
    length: 1,
    convRule: rule20
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8022,
    length: 1,
    convRule: rule20
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8114,
    length: 1,
    convRule: rule20
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8116,
    length: 1,
    convRule: rule20
  }, {
    start: 8118,
    length: 2,
    convRule: rule20
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8125,
    length: 1,
    convRule: rule10
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8127,
    length: 3,
    convRule: rule10
  }, {
    start: 8130,
    length: 1,
    convRule: rule20
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8132,
    length: 1,
    convRule: rule20
  }, {
    start: 8134,
    length: 2,
    convRule: rule20
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8141,
    length: 3,
    convRule: rule10
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8146,
    length: 2,
    convRule: rule20
  }, {
    start: 8150,
    length: 2,
    convRule: rule20
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8157,
    length: 3,
    convRule: rule10
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8162,
    length: 3,
    convRule: rule20
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8166,
    length: 2,
    convRule: rule20
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8173,
    length: 3,
    convRule: rule10
  }, {
    start: 8178,
    length: 1,
    convRule: rule20
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8180,
    length: 1,
    convRule: rule20
  }, {
    start: 8182,
    length: 2,
    convRule: rule20
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8189,
    length: 2,
    convRule: rule10
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8203,
    length: 5,
    convRule: rule16
  }, {
    start: 8208,
    length: 6,
    convRule: rule7
  }, {
    start: 8214,
    length: 2,
    convRule: rule2
  }, {
    start: 8216,
    length: 1,
    convRule: rule15
  }, {
    start: 8217,
    length: 1,
    convRule: rule19
  }, {
    start: 8218,
    length: 1,
    convRule: rule4
  }, {
    start: 8219,
    length: 2,
    convRule: rule15
  }, {
    start: 8221,
    length: 1,
    convRule: rule19
  }, {
    start: 8222,
    length: 1,
    convRule: rule4
  }, {
    start: 8223,
    length: 1,
    convRule: rule15
  }, {
    start: 8224,
    length: 8,
    convRule: rule2
  }, {
    start: 8232,
    length: 1,
    convRule: rule161
  }, {
    start: 8233,
    length: 1,
    convRule: rule162
  }, {
    start: 8234,
    length: 5,
    convRule: rule16
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8240,
    length: 9,
    convRule: rule2
  }, {
    start: 8249,
    length: 1,
    convRule: rule15
  }, {
    start: 8250,
    length: 1,
    convRule: rule19
  }, {
    start: 8251,
    length: 4,
    convRule: rule2
  }, {
    start: 8255,
    length: 2,
    convRule: rule11
  }, {
    start: 8257,
    length: 3,
    convRule: rule2
  }, {
    start: 8260,
    length: 1,
    convRule: rule6
  }, {
    start: 8261,
    length: 1,
    convRule: rule4
  }, {
    start: 8262,
    length: 1,
    convRule: rule5
  }, {
    start: 8263,
    length: 11,
    convRule: rule2
  }, {
    start: 8274,
    length: 1,
    convRule: rule6
  }, {
    start: 8275,
    length: 1,
    convRule: rule2
  }, {
    start: 8276,
    length: 1,
    convRule: rule11
  }, {
    start: 8277,
    length: 10,
    convRule: rule2
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 8288,
    length: 5,
    convRule: rule16
  }, {
    start: 8294,
    length: 10,
    convRule: rule16
  }, {
    start: 8304,
    length: 1,
    convRule: rule17
  }, {
    start: 8305,
    length: 1,
    convRule: rule91
  }, {
    start: 8308,
    length: 6,
    convRule: rule17
  }, {
    start: 8314,
    length: 3,
    convRule: rule6
  }, {
    start: 8317,
    length: 1,
    convRule: rule4
  }, {
    start: 8318,
    length: 1,
    convRule: rule5
  }, {
    start: 8319,
    length: 1,
    convRule: rule91
  }, {
    start: 8320,
    length: 10,
    convRule: rule17
  }, {
    start: 8330,
    length: 3,
    convRule: rule6
  }, {
    start: 8333,
    length: 1,
    convRule: rule4
  }, {
    start: 8334,
    length: 1,
    convRule: rule5
  }, {
    start: 8336,
    length: 13,
    convRule: rule91
  }, {
    start: 8352,
    length: 32,
    convRule: rule3
  }, {
    start: 8400,
    length: 13,
    convRule: rule92
  }, {
    start: 8413,
    length: 4,
    convRule: rule119
  }, {
    start: 8417,
    length: 1,
    convRule: rule92
  }, {
    start: 8418,
    length: 3,
    convRule: rule119
  }, {
    start: 8421,
    length: 12,
    convRule: rule92
  }, {
    start: 8448,
    length: 2,
    convRule: rule13
  }, {
    start: 8450,
    length: 1,
    convRule: rule107
  }, {
    start: 8451,
    length: 4,
    convRule: rule13
  }, {
    start: 8455,
    length: 1,
    convRule: rule107
  }, {
    start: 8456,
    length: 2,
    convRule: rule13
  }, {
    start: 8458,
    length: 1,
    convRule: rule20
  }, {
    start: 8459,
    length: 3,
    convRule: rule107
  }, {
    start: 8462,
    length: 2,
    convRule: rule20
  }, {
    start: 8464,
    length: 3,
    convRule: rule107
  }, {
    start: 8467,
    length: 1,
    convRule: rule20
  }, {
    start: 8468,
    length: 1,
    convRule: rule13
  }, {
    start: 8469,
    length: 1,
    convRule: rule107
  }, {
    start: 8470,
    length: 2,
    convRule: rule13
  }, {
    start: 8472,
    length: 1,
    convRule: rule6
  }, {
    start: 8473,
    length: 5,
    convRule: rule107
  }, {
    start: 8478,
    length: 6,
    convRule: rule13
  }, {
    start: 8484,
    length: 1,
    convRule: rule107
  }, {
    start: 8485,
    length: 1,
    convRule: rule13
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8487,
    length: 1,
    convRule: rule13
  }, {
    start: 8488,
    length: 1,
    convRule: rule107
  }, {
    start: 8489,
    length: 1,
    convRule: rule13
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8492,
    length: 2,
    convRule: rule107
  }, {
    start: 8494,
    length: 1,
    convRule: rule13
  }, {
    start: 8495,
    length: 1,
    convRule: rule20
  }, {
    start: 8496,
    length: 2,
    convRule: rule107
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8499,
    length: 1,
    convRule: rule107
  }, {
    start: 8500,
    length: 1,
    convRule: rule20
  }, {
    start: 8501,
    length: 4,
    convRule: rule14
  }, {
    start: 8505,
    length: 1,
    convRule: rule20
  }, {
    start: 8506,
    length: 2,
    convRule: rule13
  }, {
    start: 8508,
    length: 2,
    convRule: rule20
  }, {
    start: 8510,
    length: 2,
    convRule: rule107
  }, {
    start: 8512,
    length: 5,
    convRule: rule6
  }, {
    start: 8517,
    length: 1,
    convRule: rule107
  }, {
    start: 8518,
    length: 4,
    convRule: rule20
  }, {
    start: 8522,
    length: 1,
    convRule: rule13
  }, {
    start: 8523,
    length: 1,
    convRule: rule6
  }, {
    start: 8524,
    length: 2,
    convRule: rule13
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8527,
    length: 1,
    convRule: rule13
  }, {
    start: 8528,
    length: 16,
    convRule: rule17
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8576,
    length: 3,
    convRule: rule128
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 8581,
    length: 4,
    convRule: rule128
  }, {
    start: 8585,
    length: 1,
    convRule: rule17
  }, {
    start: 8586,
    length: 2,
    convRule: rule13
  }, {
    start: 8592,
    length: 5,
    convRule: rule6
  }, {
    start: 8597,
    length: 5,
    convRule: rule13
  }, {
    start: 8602,
    length: 2,
    convRule: rule6
  }, {
    start: 8604,
    length: 4,
    convRule: rule13
  }, {
    start: 8608,
    length: 1,
    convRule: rule6
  }, {
    start: 8609,
    length: 2,
    convRule: rule13
  }, {
    start: 8611,
    length: 1,
    convRule: rule6
  }, {
    start: 8612,
    length: 2,
    convRule: rule13
  }, {
    start: 8614,
    length: 1,
    convRule: rule6
  }, {
    start: 8615,
    length: 7,
    convRule: rule13
  }, {
    start: 8622,
    length: 1,
    convRule: rule6
  }, {
    start: 8623,
    length: 31,
    convRule: rule13
  }, {
    start: 8654,
    length: 2,
    convRule: rule6
  }, {
    start: 8656,
    length: 2,
    convRule: rule13
  }, {
    start: 8658,
    length: 1,
    convRule: rule6
  }, {
    start: 8659,
    length: 1,
    convRule: rule13
  }, {
    start: 8660,
    length: 1,
    convRule: rule6
  }, {
    start: 8661,
    length: 31,
    convRule: rule13
  }, {
    start: 8692,
    length: 268,
    convRule: rule6
  }, {
    start: 8960,
    length: 8,
    convRule: rule13
  }, {
    start: 8968,
    length: 1,
    convRule: rule4
  }, {
    start: 8969,
    length: 1,
    convRule: rule5
  }, {
    start: 8970,
    length: 1,
    convRule: rule4
  }, {
    start: 8971,
    length: 1,
    convRule: rule5
  }, {
    start: 8972,
    length: 20,
    convRule: rule13
  }, {
    start: 8992,
    length: 2,
    convRule: rule6
  }, {
    start: 8994,
    length: 7,
    convRule: rule13
  }, {
    start: 9001,
    length: 1,
    convRule: rule4
  }, {
    start: 9002,
    length: 1,
    convRule: rule5
  }, {
    start: 9003,
    length: 81,
    convRule: rule13
  }, {
    start: 9084,
    length: 1,
    convRule: rule6
  }, {
    start: 9085,
    length: 30,
    convRule: rule13
  }, {
    start: 9115,
    length: 25,
    convRule: rule6
  }, {
    start: 9140,
    length: 40,
    convRule: rule13
  }, {
    start: 9180,
    length: 6,
    convRule: rule6
  }, {
    start: 9186,
    length: 69,
    convRule: rule13
  }, {
    start: 9280,
    length: 11,
    convRule: rule13
  }, {
    start: 9312,
    length: 60,
    convRule: rule17
  }, {
    start: 9372,
    length: 26,
    convRule: rule13
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 9450,
    length: 22,
    convRule: rule17
  }, {
    start: 9472,
    length: 183,
    convRule: rule13
  }, {
    start: 9655,
    length: 1,
    convRule: rule6
  }, {
    start: 9656,
    length: 9,
    convRule: rule13
  }, {
    start: 9665,
    length: 1,
    convRule: rule6
  }, {
    start: 9666,
    length: 54,
    convRule: rule13
  }, {
    start: 9720,
    length: 8,
    convRule: rule6
  }, {
    start: 9728,
    length: 111,
    convRule: rule13
  }, {
    start: 9839,
    length: 1,
    convRule: rule6
  }, {
    start: 9840,
    length: 248,
    convRule: rule13
  }, {
    start: 10088,
    length: 1,
    convRule: rule4
  }, {
    start: 10089,
    length: 1,
    convRule: rule5
  }, {
    start: 10090,
    length: 1,
    convRule: rule4
  }, {
    start: 10091,
    length: 1,
    convRule: rule5
  }, {
    start: 10092,
    length: 1,
    convRule: rule4
  }, {
    start: 10093,
    length: 1,
    convRule: rule5
  }, {
    start: 10094,
    length: 1,
    convRule: rule4
  }, {
    start: 10095,
    length: 1,
    convRule: rule5
  }, {
    start: 10096,
    length: 1,
    convRule: rule4
  }, {
    start: 10097,
    length: 1,
    convRule: rule5
  }, {
    start: 10098,
    length: 1,
    convRule: rule4
  }, {
    start: 10099,
    length: 1,
    convRule: rule5
  }, {
    start: 10100,
    length: 1,
    convRule: rule4
  }, {
    start: 10101,
    length: 1,
    convRule: rule5
  }, {
    start: 10102,
    length: 30,
    convRule: rule17
  }, {
    start: 10132,
    length: 44,
    convRule: rule13
  }, {
    start: 10176,
    length: 5,
    convRule: rule6
  }, {
    start: 10181,
    length: 1,
    convRule: rule4
  }, {
    start: 10182,
    length: 1,
    convRule: rule5
  }, {
    start: 10183,
    length: 31,
    convRule: rule6
  }, {
    start: 10214,
    length: 1,
    convRule: rule4
  }, {
    start: 10215,
    length: 1,
    convRule: rule5
  }, {
    start: 10216,
    length: 1,
    convRule: rule4
  }, {
    start: 10217,
    length: 1,
    convRule: rule5
  }, {
    start: 10218,
    length: 1,
    convRule: rule4
  }, {
    start: 10219,
    length: 1,
    convRule: rule5
  }, {
    start: 10220,
    length: 1,
    convRule: rule4
  }, {
    start: 10221,
    length: 1,
    convRule: rule5
  }, {
    start: 10222,
    length: 1,
    convRule: rule4
  }, {
    start: 10223,
    length: 1,
    convRule: rule5
  }, {
    start: 10224,
    length: 16,
    convRule: rule6
  }, {
    start: 10240,
    length: 256,
    convRule: rule13
  }, {
    start: 10496,
    length: 131,
    convRule: rule6
  }, {
    start: 10627,
    length: 1,
    convRule: rule4
  }, {
    start: 10628,
    length: 1,
    convRule: rule5
  }, {
    start: 10629,
    length: 1,
    convRule: rule4
  }, {
    start: 10630,
    length: 1,
    convRule: rule5
  }, {
    start: 10631,
    length: 1,
    convRule: rule4
  }, {
    start: 10632,
    length: 1,
    convRule: rule5
  }, {
    start: 10633,
    length: 1,
    convRule: rule4
  }, {
    start: 10634,
    length: 1,
    convRule: rule5
  }, {
    start: 10635,
    length: 1,
    convRule: rule4
  }, {
    start: 10636,
    length: 1,
    convRule: rule5
  }, {
    start: 10637,
    length: 1,
    convRule: rule4
  }, {
    start: 10638,
    length: 1,
    convRule: rule5
  }, {
    start: 10639,
    length: 1,
    convRule: rule4
  }, {
    start: 10640,
    length: 1,
    convRule: rule5
  }, {
    start: 10641,
    length: 1,
    convRule: rule4
  }, {
    start: 10642,
    length: 1,
    convRule: rule5
  }, {
    start: 10643,
    length: 1,
    convRule: rule4
  }, {
    start: 10644,
    length: 1,
    convRule: rule5
  }, {
    start: 10645,
    length: 1,
    convRule: rule4
  }, {
    start: 10646,
    length: 1,
    convRule: rule5
  }, {
    start: 10647,
    length: 1,
    convRule: rule4
  }, {
    start: 10648,
    length: 1,
    convRule: rule5
  }, {
    start: 10649,
    length: 63,
    convRule: rule6
  }, {
    start: 10712,
    length: 1,
    convRule: rule4
  }, {
    start: 10713,
    length: 1,
    convRule: rule5
  }, {
    start: 10714,
    length: 1,
    convRule: rule4
  }, {
    start: 10715,
    length: 1,
    convRule: rule5
  }, {
    start: 10716,
    length: 32,
    convRule: rule6
  }, {
    start: 10748,
    length: 1,
    convRule: rule4
  }, {
    start: 10749,
    length: 1,
    convRule: rule5
  }, {
    start: 10750,
    length: 258,
    convRule: rule6
  }, {
    start: 11008,
    length: 48,
    convRule: rule13
  }, {
    start: 11056,
    length: 21,
    convRule: rule6
  }, {
    start: 11077,
    length: 2,
    convRule: rule13
  }, {
    start: 11079,
    length: 6,
    convRule: rule6
  }, {
    start: 11085,
    length: 39,
    convRule: rule13
  }, {
    start: 11126,
    length: 32,
    convRule: rule13
  }, {
    start: 11159,
    length: 105,
    convRule: rule13
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11377,
    length: 1,
    convRule: rule20
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11380,
    length: 1,
    convRule: rule20
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11383,
    length: 5,
    convRule: rule20
  }, {
    start: 11388,
    length: 2,
    convRule: rule91
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11492,
    length: 1,
    convRule: rule20
  }, {
    start: 11493,
    length: 6,
    convRule: rule13
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11503,
    length: 3,
    convRule: rule92
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11513,
    length: 4,
    convRule: rule2
  }, {
    start: 11517,
    length: 1,
    convRule: rule17
  }, {
    start: 11518,
    length: 2,
    convRule: rule2
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 11568,
    length: 56,
    convRule: rule14
  }, {
    start: 11631,
    length: 1,
    convRule: rule91
  }, {
    start: 11632,
    length: 1,
    convRule: rule2
  }, {
    start: 11647,
    length: 1,
    convRule: rule92
  }, {
    start: 11648,
    length: 23,
    convRule: rule14
  }, {
    start: 11680,
    length: 7,
    convRule: rule14
  }, {
    start: 11688,
    length: 7,
    convRule: rule14
  }, {
    start: 11696,
    length: 7,
    convRule: rule14
  }, {
    start: 11704,
    length: 7,
    convRule: rule14
  }, {
    start: 11712,
    length: 7,
    convRule: rule14
  }, {
    start: 11720,
    length: 7,
    convRule: rule14
  }, {
    start: 11728,
    length: 7,
    convRule: rule14
  }, {
    start: 11736,
    length: 7,
    convRule: rule14
  }, {
    start: 11744,
    length: 32,
    convRule: rule92
  }, {
    start: 11776,
    length: 2,
    convRule: rule2
  }, {
    start: 11778,
    length: 1,
    convRule: rule15
  }, {
    start: 11779,
    length: 1,
    convRule: rule19
  }, {
    start: 11780,
    length: 1,
    convRule: rule15
  }, {
    start: 11781,
    length: 1,
    convRule: rule19
  }, {
    start: 11782,
    length: 3,
    convRule: rule2
  }, {
    start: 11785,
    length: 1,
    convRule: rule15
  }, {
    start: 11786,
    length: 1,
    convRule: rule19
  }, {
    start: 11787,
    length: 1,
    convRule: rule2
  }, {
    start: 11788,
    length: 1,
    convRule: rule15
  }, {
    start: 11789,
    length: 1,
    convRule: rule19
  }, {
    start: 11790,
    length: 9,
    convRule: rule2
  }, {
    start: 11799,
    length: 1,
    convRule: rule7
  }, {
    start: 11800,
    length: 2,
    convRule: rule2
  }, {
    start: 11802,
    length: 1,
    convRule: rule7
  }, {
    start: 11803,
    length: 1,
    convRule: rule2
  }, {
    start: 11804,
    length: 1,
    convRule: rule15
  }, {
    start: 11805,
    length: 1,
    convRule: rule19
  }, {
    start: 11806,
    length: 2,
    convRule: rule2
  }, {
    start: 11808,
    length: 1,
    convRule: rule15
  }, {
    start: 11809,
    length: 1,
    convRule: rule19
  }, {
    start: 11810,
    length: 1,
    convRule: rule4
  }, {
    start: 11811,
    length: 1,
    convRule: rule5
  }, {
    start: 11812,
    length: 1,
    convRule: rule4
  }, {
    start: 11813,
    length: 1,
    convRule: rule5
  }, {
    start: 11814,
    length: 1,
    convRule: rule4
  }, {
    start: 11815,
    length: 1,
    convRule: rule5
  }, {
    start: 11816,
    length: 1,
    convRule: rule4
  }, {
    start: 11817,
    length: 1,
    convRule: rule5
  }, {
    start: 11818,
    length: 5,
    convRule: rule2
  }, {
    start: 11823,
    length: 1,
    convRule: rule91
  }, {
    start: 11824,
    length: 10,
    convRule: rule2
  }, {
    start: 11834,
    length: 2,
    convRule: rule7
  }, {
    start: 11836,
    length: 4,
    convRule: rule2
  }, {
    start: 11840,
    length: 1,
    convRule: rule7
  }, {
    start: 11841,
    length: 1,
    convRule: rule2
  }, {
    start: 11842,
    length: 1,
    convRule: rule4
  }, {
    start: 11843,
    length: 13,
    convRule: rule2
  }, {
    start: 11856,
    length: 2,
    convRule: rule13
  }, {
    start: 11858,
    length: 1,
    convRule: rule2
  }, {
    start: 11904,
    length: 26,
    convRule: rule13
  }, {
    start: 11931,
    length: 89,
    convRule: rule13
  }, {
    start: 12032,
    length: 214,
    convRule: rule13
  }, {
    start: 12272,
    length: 12,
    convRule: rule13
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }, {
    start: 12289,
    length: 3,
    convRule: rule2
  }, {
    start: 12292,
    length: 1,
    convRule: rule13
  }, {
    start: 12293,
    length: 1,
    convRule: rule91
  }, {
    start: 12294,
    length: 1,
    convRule: rule14
  }, {
    start: 12295,
    length: 1,
    convRule: rule128
  }, {
    start: 12296,
    length: 1,
    convRule: rule4
  }, {
    start: 12297,
    length: 1,
    convRule: rule5
  }, {
    start: 12298,
    length: 1,
    convRule: rule4
  }, {
    start: 12299,
    length: 1,
    convRule: rule5
  }, {
    start: 12300,
    length: 1,
    convRule: rule4
  }, {
    start: 12301,
    length: 1,
    convRule: rule5
  }, {
    start: 12302,
    length: 1,
    convRule: rule4
  }, {
    start: 12303,
    length: 1,
    convRule: rule5
  }, {
    start: 12304,
    length: 1,
    convRule: rule4
  }, {
    start: 12305,
    length: 1,
    convRule: rule5
  }, {
    start: 12306,
    length: 2,
    convRule: rule13
  }, {
    start: 12308,
    length: 1,
    convRule: rule4
  }, {
    start: 12309,
    length: 1,
    convRule: rule5
  }, {
    start: 12310,
    length: 1,
    convRule: rule4
  }, {
    start: 12311,
    length: 1,
    convRule: rule5
  }, {
    start: 12312,
    length: 1,
    convRule: rule4
  }, {
    start: 12313,
    length: 1,
    convRule: rule5
  }, {
    start: 12314,
    length: 1,
    convRule: rule4
  }, {
    start: 12315,
    length: 1,
    convRule: rule5
  }, {
    start: 12316,
    length: 1,
    convRule: rule7
  }, {
    start: 12317,
    length: 1,
    convRule: rule4
  }, {
    start: 12318,
    length: 2,
    convRule: rule5
  }, {
    start: 12320,
    length: 1,
    convRule: rule13
  }, {
    start: 12321,
    length: 9,
    convRule: rule128
  }, {
    start: 12330,
    length: 4,
    convRule: rule92
  }, {
    start: 12334,
    length: 2,
    convRule: rule124
  }, {
    start: 12336,
    length: 1,
    convRule: rule7
  }, {
    start: 12337,
    length: 5,
    convRule: rule91
  }, {
    start: 12342,
    length: 2,
    convRule: rule13
  }, {
    start: 12344,
    length: 3,
    convRule: rule128
  }, {
    start: 12347,
    length: 1,
    convRule: rule91
  }, {
    start: 12348,
    length: 1,
    convRule: rule14
  }, {
    start: 12349,
    length: 1,
    convRule: rule2
  }, {
    start: 12350,
    length: 2,
    convRule: rule13
  }, {
    start: 12353,
    length: 86,
    convRule: rule14
  }, {
    start: 12441,
    length: 2,
    convRule: rule92
  }, {
    start: 12443,
    length: 2,
    convRule: rule10
  }, {
    start: 12445,
    length: 2,
    convRule: rule91
  }, {
    start: 12447,
    length: 1,
    convRule: rule14
  }, {
    start: 12448,
    length: 1,
    convRule: rule7
  }, {
    start: 12449,
    length: 90,
    convRule: rule14
  }, {
    start: 12539,
    length: 1,
    convRule: rule2
  }, {
    start: 12540,
    length: 3,
    convRule: rule91
  }, {
    start: 12543,
    length: 1,
    convRule: rule14
  }, {
    start: 12549,
    length: 43,
    convRule: rule14
  }, {
    start: 12593,
    length: 94,
    convRule: rule14
  }, {
    start: 12688,
    length: 2,
    convRule: rule13
  }, {
    start: 12690,
    length: 4,
    convRule: rule17
  }, {
    start: 12694,
    length: 10,
    convRule: rule13
  }, {
    start: 12704,
    length: 32,
    convRule: rule14
  }, {
    start: 12736,
    length: 36,
    convRule: rule13
  }, {
    start: 12784,
    length: 16,
    convRule: rule14
  }, {
    start: 12800,
    length: 31,
    convRule: rule13
  }, {
    start: 12832,
    length: 10,
    convRule: rule17
  }, {
    start: 12842,
    length: 30,
    convRule: rule13
  }, {
    start: 12872,
    length: 8,
    convRule: rule17
  }, {
    start: 12880,
    length: 1,
    convRule: rule13
  }, {
    start: 12881,
    length: 15,
    convRule: rule17
  }, {
    start: 12896,
    length: 32,
    convRule: rule13
  }, {
    start: 12928,
    length: 10,
    convRule: rule17
  }, {
    start: 12938,
    length: 39,
    convRule: rule13
  }, {
    start: 12977,
    length: 15,
    convRule: rule17
  }, {
    start: 12992,
    length: 320,
    convRule: rule13
  }, {
    start: 13312,
    length: 6592,
    convRule: rule14
  }, {
    start: 19904,
    length: 64,
    convRule: rule13
  }, {
    start: 19968,
    length: 20989,
    convRule: rule14
  }, {
    start: 40960,
    length: 21,
    convRule: rule14
  }, {
    start: 40981,
    length: 1,
    convRule: rule91
  }, {
    start: 40982,
    length: 1143,
    convRule: rule14
  }, {
    start: 42128,
    length: 55,
    convRule: rule13
  }, {
    start: 42192,
    length: 40,
    convRule: rule14
  }, {
    start: 42232,
    length: 6,
    convRule: rule91
  }, {
    start: 42238,
    length: 2,
    convRule: rule2
  }, {
    start: 42240,
    length: 268,
    convRule: rule14
  }, {
    start: 42508,
    length: 1,
    convRule: rule91
  }, {
    start: 42509,
    length: 3,
    convRule: rule2
  }, {
    start: 42512,
    length: 16,
    convRule: rule14
  }, {
    start: 42528,
    length: 10,
    convRule: rule8
  }, {
    start: 42538,
    length: 2,
    convRule: rule14
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42606,
    length: 1,
    convRule: rule14
  }, {
    start: 42607,
    length: 1,
    convRule: rule92
  }, {
    start: 42608,
    length: 3,
    convRule: rule119
  }, {
    start: 42611,
    length: 1,
    convRule: rule2
  }, {
    start: 42612,
    length: 10,
    convRule: rule92
  }, {
    start: 42622,
    length: 1,
    convRule: rule2
  }, {
    start: 42623,
    length: 1,
    convRule: rule91
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42652,
    length: 2,
    convRule: rule91
  }, {
    start: 42654,
    length: 2,
    convRule: rule92
  }, {
    start: 42656,
    length: 70,
    convRule: rule14
  }, {
    start: 42726,
    length: 10,
    convRule: rule128
  }, {
    start: 42736,
    length: 2,
    convRule: rule92
  }, {
    start: 42738,
    length: 6,
    convRule: rule2
  }, {
    start: 42752,
    length: 23,
    convRule: rule10
  }, {
    start: 42775,
    length: 9,
    convRule: rule91
  }, {
    start: 42784,
    length: 2,
    convRule: rule10
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42800,
    length: 2,
    convRule: rule20
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42864,
    length: 1,
    convRule: rule91
  }, {
    start: 42865,
    length: 8,
    convRule: rule20
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42888,
    length: 1,
    convRule: rule91
  }, {
    start: 42889,
    length: 2,
    convRule: rule10
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42894,
    length: 1,
    convRule: rule20
  }, {
    start: 42895,
    length: 1,
    convRule: rule14
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42901,
    length: 1,
    convRule: rule20
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42927,
    length: 1,
    convRule: rule20
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 42999,
    length: 1,
    convRule: rule14
  }, {
    start: 43e3,
    length: 2,
    convRule: rule91
  }, {
    start: 43002,
    length: 1,
    convRule: rule20
  }, {
    start: 43003,
    length: 7,
    convRule: rule14
  }, {
    start: 43010,
    length: 1,
    convRule: rule92
  }, {
    start: 43011,
    length: 3,
    convRule: rule14
  }, {
    start: 43014,
    length: 1,
    convRule: rule92
  }, {
    start: 43015,
    length: 4,
    convRule: rule14
  }, {
    start: 43019,
    length: 1,
    convRule: rule92
  }, {
    start: 43020,
    length: 23,
    convRule: rule14
  }, {
    start: 43043,
    length: 2,
    convRule: rule124
  }, {
    start: 43045,
    length: 2,
    convRule: rule92
  }, {
    start: 43047,
    length: 1,
    convRule: rule124
  }, {
    start: 43048,
    length: 4,
    convRule: rule13
  }, {
    start: 43052,
    length: 1,
    convRule: rule92
  }, {
    start: 43056,
    length: 6,
    convRule: rule17
  }, {
    start: 43062,
    length: 2,
    convRule: rule13
  }, {
    start: 43064,
    length: 1,
    convRule: rule3
  }, {
    start: 43065,
    length: 1,
    convRule: rule13
  }, {
    start: 43072,
    length: 52,
    convRule: rule14
  }, {
    start: 43124,
    length: 4,
    convRule: rule2
  }, {
    start: 43136,
    length: 2,
    convRule: rule124
  }, {
    start: 43138,
    length: 50,
    convRule: rule14
  }, {
    start: 43188,
    length: 16,
    convRule: rule124
  }, {
    start: 43204,
    length: 2,
    convRule: rule92
  }, {
    start: 43214,
    length: 2,
    convRule: rule2
  }, {
    start: 43216,
    length: 10,
    convRule: rule8
  }, {
    start: 43232,
    length: 18,
    convRule: rule92
  }, {
    start: 43250,
    length: 6,
    convRule: rule14
  }, {
    start: 43256,
    length: 3,
    convRule: rule2
  }, {
    start: 43259,
    length: 1,
    convRule: rule14
  }, {
    start: 43260,
    length: 1,
    convRule: rule2
  }, {
    start: 43261,
    length: 2,
    convRule: rule14
  }, {
    start: 43263,
    length: 1,
    convRule: rule92
  }, {
    start: 43264,
    length: 10,
    convRule: rule8
  }, {
    start: 43274,
    length: 28,
    convRule: rule14
  }, {
    start: 43302,
    length: 8,
    convRule: rule92
  }, {
    start: 43310,
    length: 2,
    convRule: rule2
  }, {
    start: 43312,
    length: 23,
    convRule: rule14
  }, {
    start: 43335,
    length: 11,
    convRule: rule92
  }, {
    start: 43346,
    length: 2,
    convRule: rule124
  }, {
    start: 43359,
    length: 1,
    convRule: rule2
  }, {
    start: 43360,
    length: 29,
    convRule: rule14
  }, {
    start: 43392,
    length: 3,
    convRule: rule92
  }, {
    start: 43395,
    length: 1,
    convRule: rule124
  }, {
    start: 43396,
    length: 47,
    convRule: rule14
  }, {
    start: 43443,
    length: 1,
    convRule: rule92
  }, {
    start: 43444,
    length: 2,
    convRule: rule124
  }, {
    start: 43446,
    length: 4,
    convRule: rule92
  }, {
    start: 43450,
    length: 2,
    convRule: rule124
  }, {
    start: 43452,
    length: 2,
    convRule: rule92
  }, {
    start: 43454,
    length: 3,
    convRule: rule124
  }, {
    start: 43457,
    length: 13,
    convRule: rule2
  }, {
    start: 43471,
    length: 1,
    convRule: rule91
  }, {
    start: 43472,
    length: 10,
    convRule: rule8
  }, {
    start: 43486,
    length: 2,
    convRule: rule2
  }, {
    start: 43488,
    length: 5,
    convRule: rule14
  }, {
    start: 43493,
    length: 1,
    convRule: rule92
  }, {
    start: 43494,
    length: 1,
    convRule: rule91
  }, {
    start: 43495,
    length: 9,
    convRule: rule14
  }, {
    start: 43504,
    length: 10,
    convRule: rule8
  }, {
    start: 43514,
    length: 5,
    convRule: rule14
  }, {
    start: 43520,
    length: 41,
    convRule: rule14
  }, {
    start: 43561,
    length: 6,
    convRule: rule92
  }, {
    start: 43567,
    length: 2,
    convRule: rule124
  }, {
    start: 43569,
    length: 2,
    convRule: rule92
  }, {
    start: 43571,
    length: 2,
    convRule: rule124
  }, {
    start: 43573,
    length: 2,
    convRule: rule92
  }, {
    start: 43584,
    length: 3,
    convRule: rule14
  }, {
    start: 43587,
    length: 1,
    convRule: rule92
  }, {
    start: 43588,
    length: 8,
    convRule: rule14
  }, {
    start: 43596,
    length: 1,
    convRule: rule92
  }, {
    start: 43597,
    length: 1,
    convRule: rule124
  }, {
    start: 43600,
    length: 10,
    convRule: rule8
  }, {
    start: 43612,
    length: 4,
    convRule: rule2
  }, {
    start: 43616,
    length: 16,
    convRule: rule14
  }, {
    start: 43632,
    length: 1,
    convRule: rule91
  }, {
    start: 43633,
    length: 6,
    convRule: rule14
  }, {
    start: 43639,
    length: 3,
    convRule: rule13
  }, {
    start: 43642,
    length: 1,
    convRule: rule14
  }, {
    start: 43643,
    length: 1,
    convRule: rule124
  }, {
    start: 43644,
    length: 1,
    convRule: rule92
  }, {
    start: 43645,
    length: 1,
    convRule: rule124
  }, {
    start: 43646,
    length: 50,
    convRule: rule14
  }, {
    start: 43696,
    length: 1,
    convRule: rule92
  }, {
    start: 43697,
    length: 1,
    convRule: rule14
  }, {
    start: 43698,
    length: 3,
    convRule: rule92
  }, {
    start: 43701,
    length: 2,
    convRule: rule14
  }, {
    start: 43703,
    length: 2,
    convRule: rule92
  }, {
    start: 43705,
    length: 5,
    convRule: rule14
  }, {
    start: 43710,
    length: 2,
    convRule: rule92
  }, {
    start: 43712,
    length: 1,
    convRule: rule14
  }, {
    start: 43713,
    length: 1,
    convRule: rule92
  }, {
    start: 43714,
    length: 1,
    convRule: rule14
  }, {
    start: 43739,
    length: 2,
    convRule: rule14
  }, {
    start: 43741,
    length: 1,
    convRule: rule91
  }, {
    start: 43742,
    length: 2,
    convRule: rule2
  }, {
    start: 43744,
    length: 11,
    convRule: rule14
  }, {
    start: 43755,
    length: 1,
    convRule: rule124
  }, {
    start: 43756,
    length: 2,
    convRule: rule92
  }, {
    start: 43758,
    length: 2,
    convRule: rule124
  }, {
    start: 43760,
    length: 2,
    convRule: rule2
  }, {
    start: 43762,
    length: 1,
    convRule: rule14
  }, {
    start: 43763,
    length: 2,
    convRule: rule91
  }, {
    start: 43765,
    length: 1,
    convRule: rule124
  }, {
    start: 43766,
    length: 1,
    convRule: rule92
  }, {
    start: 43777,
    length: 6,
    convRule: rule14
  }, {
    start: 43785,
    length: 6,
    convRule: rule14
  }, {
    start: 43793,
    length: 6,
    convRule: rule14
  }, {
    start: 43808,
    length: 7,
    convRule: rule14
  }, {
    start: 43816,
    length: 7,
    convRule: rule14
  }, {
    start: 43824,
    length: 35,
    convRule: rule20
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43860,
    length: 7,
    convRule: rule20
  }, {
    start: 43867,
    length: 1,
    convRule: rule10
  }, {
    start: 43868,
    length: 4,
    convRule: rule91
  }, {
    start: 43872,
    length: 9,
    convRule: rule20
  }, {
    start: 43881,
    length: 1,
    convRule: rule91
  }, {
    start: 43882,
    length: 2,
    convRule: rule10
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 43968,
    length: 35,
    convRule: rule14
  }, {
    start: 44003,
    length: 2,
    convRule: rule124
  }, {
    start: 44005,
    length: 1,
    convRule: rule92
  }, {
    start: 44006,
    length: 2,
    convRule: rule124
  }, {
    start: 44008,
    length: 1,
    convRule: rule92
  }, {
    start: 44009,
    length: 2,
    convRule: rule124
  }, {
    start: 44011,
    length: 1,
    convRule: rule2
  }, {
    start: 44012,
    length: 1,
    convRule: rule124
  }, {
    start: 44013,
    length: 1,
    convRule: rule92
  }, {
    start: 44016,
    length: 10,
    convRule: rule8
  }, {
    start: 44032,
    length: 11172,
    convRule: rule14
  }, {
    start: 55216,
    length: 23,
    convRule: rule14
  }, {
    start: 55243,
    length: 49,
    convRule: rule14
  }, {
    start: 55296,
    length: 896,
    convRule: rule199
  }, {
    start: 56192,
    length: 128,
    convRule: rule199
  }, {
    start: 56320,
    length: 1024,
    convRule: rule199
  }, {
    start: 57344,
    length: 6400,
    convRule: rule200
  }, {
    start: 63744,
    length: 366,
    convRule: rule14
  }, {
    start: 64112,
    length: 106,
    convRule: rule14
  }, {
    start: 64256,
    length: 7,
    convRule: rule20
  }, {
    start: 64275,
    length: 5,
    convRule: rule20
  }, {
    start: 64285,
    length: 1,
    convRule: rule14
  }, {
    start: 64286,
    length: 1,
    convRule: rule92
  }, {
    start: 64287,
    length: 10,
    convRule: rule14
  }, {
    start: 64297,
    length: 1,
    convRule: rule6
  }, {
    start: 64298,
    length: 13,
    convRule: rule14
  }, {
    start: 64312,
    length: 5,
    convRule: rule14
  }, {
    start: 64318,
    length: 1,
    convRule: rule14
  }, {
    start: 64320,
    length: 2,
    convRule: rule14
  }, {
    start: 64323,
    length: 2,
    convRule: rule14
  }, {
    start: 64326,
    length: 108,
    convRule: rule14
  }, {
    start: 64434,
    length: 16,
    convRule: rule10
  }, {
    start: 64467,
    length: 363,
    convRule: rule14
  }, {
    start: 64830,
    length: 1,
    convRule: rule5
  }, {
    start: 64831,
    length: 1,
    convRule: rule4
  }, {
    start: 64848,
    length: 64,
    convRule: rule14
  }, {
    start: 64914,
    length: 54,
    convRule: rule14
  }, {
    start: 65008,
    length: 12,
    convRule: rule14
  }, {
    start: 65020,
    length: 1,
    convRule: rule3
  }, {
    start: 65021,
    length: 1,
    convRule: rule13
  }, {
    start: 65024,
    length: 16,
    convRule: rule92
  }, {
    start: 65040,
    length: 7,
    convRule: rule2
  }, {
    start: 65047,
    length: 1,
    convRule: rule4
  }, {
    start: 65048,
    length: 1,
    convRule: rule5
  }, {
    start: 65049,
    length: 1,
    convRule: rule2
  }, {
    start: 65056,
    length: 16,
    convRule: rule92
  }, {
    start: 65072,
    length: 1,
    convRule: rule2
  }, {
    start: 65073,
    length: 2,
    convRule: rule7
  }, {
    start: 65075,
    length: 2,
    convRule: rule11
  }, {
    start: 65077,
    length: 1,
    convRule: rule4
  }, {
    start: 65078,
    length: 1,
    convRule: rule5
  }, {
    start: 65079,
    length: 1,
    convRule: rule4
  }, {
    start: 65080,
    length: 1,
    convRule: rule5
  }, {
    start: 65081,
    length: 1,
    convRule: rule4
  }, {
    start: 65082,
    length: 1,
    convRule: rule5
  }, {
    start: 65083,
    length: 1,
    convRule: rule4
  }, {
    start: 65084,
    length: 1,
    convRule: rule5
  }, {
    start: 65085,
    length: 1,
    convRule: rule4
  }, {
    start: 65086,
    length: 1,
    convRule: rule5
  }, {
    start: 65087,
    length: 1,
    convRule: rule4
  }, {
    start: 65088,
    length: 1,
    convRule: rule5
  }, {
    start: 65089,
    length: 1,
    convRule: rule4
  }, {
    start: 65090,
    length: 1,
    convRule: rule5
  }, {
    start: 65091,
    length: 1,
    convRule: rule4
  }, {
    start: 65092,
    length: 1,
    convRule: rule5
  }, {
    start: 65093,
    length: 2,
    convRule: rule2
  }, {
    start: 65095,
    length: 1,
    convRule: rule4
  }, {
    start: 65096,
    length: 1,
    convRule: rule5
  }, {
    start: 65097,
    length: 4,
    convRule: rule2
  }, {
    start: 65101,
    length: 3,
    convRule: rule11
  }, {
    start: 65104,
    length: 3,
    convRule: rule2
  }, {
    start: 65108,
    length: 4,
    convRule: rule2
  }, {
    start: 65112,
    length: 1,
    convRule: rule7
  }, {
    start: 65113,
    length: 1,
    convRule: rule4
  }, {
    start: 65114,
    length: 1,
    convRule: rule5
  }, {
    start: 65115,
    length: 1,
    convRule: rule4
  }, {
    start: 65116,
    length: 1,
    convRule: rule5
  }, {
    start: 65117,
    length: 1,
    convRule: rule4
  }, {
    start: 65118,
    length: 1,
    convRule: rule5
  }, {
    start: 65119,
    length: 3,
    convRule: rule2
  }, {
    start: 65122,
    length: 1,
    convRule: rule6
  }, {
    start: 65123,
    length: 1,
    convRule: rule7
  }, {
    start: 65124,
    length: 3,
    convRule: rule6
  }, {
    start: 65128,
    length: 1,
    convRule: rule2
  }, {
    start: 65129,
    length: 1,
    convRule: rule3
  }, {
    start: 65130,
    length: 2,
    convRule: rule2
  }, {
    start: 65136,
    length: 5,
    convRule: rule14
  }, {
    start: 65142,
    length: 135,
    convRule: rule14
  }, {
    start: 65279,
    length: 1,
    convRule: rule16
  }, {
    start: 65281,
    length: 3,
    convRule: rule2
  }, {
    start: 65284,
    length: 1,
    convRule: rule3
  }, {
    start: 65285,
    length: 3,
    convRule: rule2
  }, {
    start: 65288,
    length: 1,
    convRule: rule4
  }, {
    start: 65289,
    length: 1,
    convRule: rule5
  }, {
    start: 65290,
    length: 1,
    convRule: rule2
  }, {
    start: 65291,
    length: 1,
    convRule: rule6
  }, {
    start: 65292,
    length: 1,
    convRule: rule2
  }, {
    start: 65293,
    length: 1,
    convRule: rule7
  }, {
    start: 65294,
    length: 2,
    convRule: rule2
  }, {
    start: 65296,
    length: 10,
    convRule: rule8
  }, {
    start: 65306,
    length: 2,
    convRule: rule2
  }, {
    start: 65308,
    length: 3,
    convRule: rule6
  }, {
    start: 65311,
    length: 2,
    convRule: rule2
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65339,
    length: 1,
    convRule: rule4
  }, {
    start: 65340,
    length: 1,
    convRule: rule2
  }, {
    start: 65341,
    length: 1,
    convRule: rule5
  }, {
    start: 65342,
    length: 1,
    convRule: rule10
  }, {
    start: 65343,
    length: 1,
    convRule: rule11
  }, {
    start: 65344,
    length: 1,
    convRule: rule10
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 65371,
    length: 1,
    convRule: rule4
  }, {
    start: 65372,
    length: 1,
    convRule: rule6
  }, {
    start: 65373,
    length: 1,
    convRule: rule5
  }, {
    start: 65374,
    length: 1,
    convRule: rule6
  }, {
    start: 65375,
    length: 1,
    convRule: rule4
  }, {
    start: 65376,
    length: 1,
    convRule: rule5
  }, {
    start: 65377,
    length: 1,
    convRule: rule2
  }, {
    start: 65378,
    length: 1,
    convRule: rule4
  }, {
    start: 65379,
    length: 1,
    convRule: rule5
  }, {
    start: 65380,
    length: 2,
    convRule: rule2
  }, {
    start: 65382,
    length: 10,
    convRule: rule14
  }, {
    start: 65392,
    length: 1,
    convRule: rule91
  }, {
    start: 65393,
    length: 45,
    convRule: rule14
  }, {
    start: 65438,
    length: 2,
    convRule: rule91
  }, {
    start: 65440,
    length: 31,
    convRule: rule14
  }, {
    start: 65474,
    length: 6,
    convRule: rule14
  }, {
    start: 65482,
    length: 6,
    convRule: rule14
  }, {
    start: 65490,
    length: 6,
    convRule: rule14
  }, {
    start: 65498,
    length: 3,
    convRule: rule14
  }, {
    start: 65504,
    length: 2,
    convRule: rule3
  }, {
    start: 65506,
    length: 1,
    convRule: rule6
  }, {
    start: 65507,
    length: 1,
    convRule: rule10
  }, {
    start: 65508,
    length: 1,
    convRule: rule13
  }, {
    start: 65509,
    length: 2,
    convRule: rule3
  }, {
    start: 65512,
    length: 1,
    convRule: rule13
  }, {
    start: 65513,
    length: 4,
    convRule: rule6
  }, {
    start: 65517,
    length: 2,
    convRule: rule13
  }, {
    start: 65529,
    length: 3,
    convRule: rule16
  }, {
    start: 65532,
    length: 2,
    convRule: rule13
  }, {
    start: 65536,
    length: 12,
    convRule: rule14
  }, {
    start: 65549,
    length: 26,
    convRule: rule14
  }, {
    start: 65576,
    length: 19,
    convRule: rule14
  }, {
    start: 65596,
    length: 2,
    convRule: rule14
  }, {
    start: 65599,
    length: 15,
    convRule: rule14
  }, {
    start: 65616,
    length: 14,
    convRule: rule14
  }, {
    start: 65664,
    length: 123,
    convRule: rule14
  }, {
    start: 65792,
    length: 3,
    convRule: rule2
  }, {
    start: 65799,
    length: 45,
    convRule: rule17
  }, {
    start: 65847,
    length: 9,
    convRule: rule13
  }, {
    start: 65856,
    length: 53,
    convRule: rule128
  }, {
    start: 65909,
    length: 4,
    convRule: rule17
  }, {
    start: 65913,
    length: 17,
    convRule: rule13
  }, {
    start: 65930,
    length: 2,
    convRule: rule17
  }, {
    start: 65932,
    length: 3,
    convRule: rule13
  }, {
    start: 65936,
    length: 13,
    convRule: rule13
  }, {
    start: 65952,
    length: 1,
    convRule: rule13
  }, {
    start: 66e3,
    length: 45,
    convRule: rule13
  }, {
    start: 66045,
    length: 1,
    convRule: rule92
  }, {
    start: 66176,
    length: 29,
    convRule: rule14
  }, {
    start: 66208,
    length: 49,
    convRule: rule14
  }, {
    start: 66272,
    length: 1,
    convRule: rule92
  }, {
    start: 66273,
    length: 27,
    convRule: rule17
  }, {
    start: 66304,
    length: 32,
    convRule: rule14
  }, {
    start: 66336,
    length: 4,
    convRule: rule17
  }, {
    start: 66349,
    length: 20,
    convRule: rule14
  }, {
    start: 66369,
    length: 1,
    convRule: rule128
  }, {
    start: 66370,
    length: 8,
    convRule: rule14
  }, {
    start: 66378,
    length: 1,
    convRule: rule128
  }, {
    start: 66384,
    length: 38,
    convRule: rule14
  }, {
    start: 66422,
    length: 5,
    convRule: rule92
  }, {
    start: 66432,
    length: 30,
    convRule: rule14
  }, {
    start: 66463,
    length: 1,
    convRule: rule2
  }, {
    start: 66464,
    length: 36,
    convRule: rule14
  }, {
    start: 66504,
    length: 8,
    convRule: rule14
  }, {
    start: 66512,
    length: 1,
    convRule: rule2
  }, {
    start: 66513,
    length: 5,
    convRule: rule128
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66640,
    length: 78,
    convRule: rule14
  }, {
    start: 66720,
    length: 10,
    convRule: rule8
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 66816,
    length: 40,
    convRule: rule14
  }, {
    start: 66864,
    length: 52,
    convRule: rule14
  }, {
    start: 66927,
    length: 1,
    convRule: rule2
  }, {
    start: 67072,
    length: 311,
    convRule: rule14
  }, {
    start: 67392,
    length: 22,
    convRule: rule14
  }, {
    start: 67424,
    length: 8,
    convRule: rule14
  }, {
    start: 67584,
    length: 6,
    convRule: rule14
  }, {
    start: 67592,
    length: 1,
    convRule: rule14
  }, {
    start: 67594,
    length: 44,
    convRule: rule14
  }, {
    start: 67639,
    length: 2,
    convRule: rule14
  }, {
    start: 67644,
    length: 1,
    convRule: rule14
  }, {
    start: 67647,
    length: 23,
    convRule: rule14
  }, {
    start: 67671,
    length: 1,
    convRule: rule2
  }, {
    start: 67672,
    length: 8,
    convRule: rule17
  }, {
    start: 67680,
    length: 23,
    convRule: rule14
  }, {
    start: 67703,
    length: 2,
    convRule: rule13
  }, {
    start: 67705,
    length: 7,
    convRule: rule17
  }, {
    start: 67712,
    length: 31,
    convRule: rule14
  }, {
    start: 67751,
    length: 9,
    convRule: rule17
  }, {
    start: 67808,
    length: 19,
    convRule: rule14
  }, {
    start: 67828,
    length: 2,
    convRule: rule14
  }, {
    start: 67835,
    length: 5,
    convRule: rule17
  }, {
    start: 67840,
    length: 22,
    convRule: rule14
  }, {
    start: 67862,
    length: 6,
    convRule: rule17
  }, {
    start: 67871,
    length: 1,
    convRule: rule2
  }, {
    start: 67872,
    length: 26,
    convRule: rule14
  }, {
    start: 67903,
    length: 1,
    convRule: rule2
  }, {
    start: 67968,
    length: 56,
    convRule: rule14
  }, {
    start: 68028,
    length: 2,
    convRule: rule17
  }, {
    start: 68030,
    length: 2,
    convRule: rule14
  }, {
    start: 68032,
    length: 16,
    convRule: rule17
  }, {
    start: 68050,
    length: 46,
    convRule: rule17
  }, {
    start: 68096,
    length: 1,
    convRule: rule14
  }, {
    start: 68097,
    length: 3,
    convRule: rule92
  }, {
    start: 68101,
    length: 2,
    convRule: rule92
  }, {
    start: 68108,
    length: 4,
    convRule: rule92
  }, {
    start: 68112,
    length: 4,
    convRule: rule14
  }, {
    start: 68117,
    length: 3,
    convRule: rule14
  }, {
    start: 68121,
    length: 29,
    convRule: rule14
  }, {
    start: 68152,
    length: 3,
    convRule: rule92
  }, {
    start: 68159,
    length: 1,
    convRule: rule92
  }, {
    start: 68160,
    length: 9,
    convRule: rule17
  }, {
    start: 68176,
    length: 9,
    convRule: rule2
  }, {
    start: 68192,
    length: 29,
    convRule: rule14
  }, {
    start: 68221,
    length: 2,
    convRule: rule17
  }, {
    start: 68223,
    length: 1,
    convRule: rule2
  }, {
    start: 68224,
    length: 29,
    convRule: rule14
  }, {
    start: 68253,
    length: 3,
    convRule: rule17
  }, {
    start: 68288,
    length: 8,
    convRule: rule14
  }, {
    start: 68296,
    length: 1,
    convRule: rule13
  }, {
    start: 68297,
    length: 28,
    convRule: rule14
  }, {
    start: 68325,
    length: 2,
    convRule: rule92
  }, {
    start: 68331,
    length: 5,
    convRule: rule17
  }, {
    start: 68336,
    length: 7,
    convRule: rule2
  }, {
    start: 68352,
    length: 54,
    convRule: rule14
  }, {
    start: 68409,
    length: 7,
    convRule: rule2
  }, {
    start: 68416,
    length: 22,
    convRule: rule14
  }, {
    start: 68440,
    length: 8,
    convRule: rule17
  }, {
    start: 68448,
    length: 19,
    convRule: rule14
  }, {
    start: 68472,
    length: 8,
    convRule: rule17
  }, {
    start: 68480,
    length: 18,
    convRule: rule14
  }, {
    start: 68505,
    length: 4,
    convRule: rule2
  }, {
    start: 68521,
    length: 7,
    convRule: rule17
  }, {
    start: 68608,
    length: 73,
    convRule: rule14
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 68858,
    length: 6,
    convRule: rule17
  }, {
    start: 68864,
    length: 36,
    convRule: rule14
  }, {
    start: 68900,
    length: 4,
    convRule: rule92
  }, {
    start: 68912,
    length: 10,
    convRule: rule8
  }, {
    start: 69216,
    length: 31,
    convRule: rule17
  }, {
    start: 69248,
    length: 42,
    convRule: rule14
  }, {
    start: 69291,
    length: 2,
    convRule: rule92
  }, {
    start: 69293,
    length: 1,
    convRule: rule7
  }, {
    start: 69296,
    length: 2,
    convRule: rule14
  }, {
    start: 69376,
    length: 29,
    convRule: rule14
  }, {
    start: 69405,
    length: 10,
    convRule: rule17
  }, {
    start: 69415,
    length: 1,
    convRule: rule14
  }, {
    start: 69424,
    length: 22,
    convRule: rule14
  }, {
    start: 69446,
    length: 11,
    convRule: rule92
  }, {
    start: 69457,
    length: 4,
    convRule: rule17
  }, {
    start: 69461,
    length: 5,
    convRule: rule2
  }, {
    start: 69552,
    length: 21,
    convRule: rule14
  }, {
    start: 69573,
    length: 7,
    convRule: rule17
  }, {
    start: 69600,
    length: 23,
    convRule: rule14
  }, {
    start: 69632,
    length: 1,
    convRule: rule124
  }, {
    start: 69633,
    length: 1,
    convRule: rule92
  }, {
    start: 69634,
    length: 1,
    convRule: rule124
  }, {
    start: 69635,
    length: 53,
    convRule: rule14
  }, {
    start: 69688,
    length: 15,
    convRule: rule92
  }, {
    start: 69703,
    length: 7,
    convRule: rule2
  }, {
    start: 69714,
    length: 20,
    convRule: rule17
  }, {
    start: 69734,
    length: 10,
    convRule: rule8
  }, {
    start: 69759,
    length: 3,
    convRule: rule92
  }, {
    start: 69762,
    length: 1,
    convRule: rule124
  }, {
    start: 69763,
    length: 45,
    convRule: rule14
  }, {
    start: 69808,
    length: 3,
    convRule: rule124
  }, {
    start: 69811,
    length: 4,
    convRule: rule92
  }, {
    start: 69815,
    length: 2,
    convRule: rule124
  }, {
    start: 69817,
    length: 2,
    convRule: rule92
  }, {
    start: 69819,
    length: 2,
    convRule: rule2
  }, {
    start: 69821,
    length: 1,
    convRule: rule16
  }, {
    start: 69822,
    length: 4,
    convRule: rule2
  }, {
    start: 69837,
    length: 1,
    convRule: rule16
  }, {
    start: 69840,
    length: 25,
    convRule: rule14
  }, {
    start: 69872,
    length: 10,
    convRule: rule8
  }, {
    start: 69888,
    length: 3,
    convRule: rule92
  }, {
    start: 69891,
    length: 36,
    convRule: rule14
  }, {
    start: 69927,
    length: 5,
    convRule: rule92
  }, {
    start: 69932,
    length: 1,
    convRule: rule124
  }, {
    start: 69933,
    length: 8,
    convRule: rule92
  }, {
    start: 69942,
    length: 10,
    convRule: rule8
  }, {
    start: 69952,
    length: 4,
    convRule: rule2
  }, {
    start: 69956,
    length: 1,
    convRule: rule14
  }, {
    start: 69957,
    length: 2,
    convRule: rule124
  }, {
    start: 69959,
    length: 1,
    convRule: rule14
  }, {
    start: 69968,
    length: 35,
    convRule: rule14
  }, {
    start: 70003,
    length: 1,
    convRule: rule92
  }, {
    start: 70004,
    length: 2,
    convRule: rule2
  }, {
    start: 70006,
    length: 1,
    convRule: rule14
  }, {
    start: 70016,
    length: 2,
    convRule: rule92
  }, {
    start: 70018,
    length: 1,
    convRule: rule124
  }, {
    start: 70019,
    length: 48,
    convRule: rule14
  }, {
    start: 70067,
    length: 3,
    convRule: rule124
  }, {
    start: 70070,
    length: 9,
    convRule: rule92
  }, {
    start: 70079,
    length: 2,
    convRule: rule124
  }, {
    start: 70081,
    length: 4,
    convRule: rule14
  }, {
    start: 70085,
    length: 4,
    convRule: rule2
  }, {
    start: 70089,
    length: 4,
    convRule: rule92
  }, {
    start: 70093,
    length: 1,
    convRule: rule2
  }, {
    start: 70094,
    length: 1,
    convRule: rule124
  }, {
    start: 70095,
    length: 1,
    convRule: rule92
  }, {
    start: 70096,
    length: 10,
    convRule: rule8
  }, {
    start: 70106,
    length: 1,
    convRule: rule14
  }, {
    start: 70107,
    length: 1,
    convRule: rule2
  }, {
    start: 70108,
    length: 1,
    convRule: rule14
  }, {
    start: 70109,
    length: 3,
    convRule: rule2
  }, {
    start: 70113,
    length: 20,
    convRule: rule17
  }, {
    start: 70144,
    length: 18,
    convRule: rule14
  }, {
    start: 70163,
    length: 25,
    convRule: rule14
  }, {
    start: 70188,
    length: 3,
    convRule: rule124
  }, {
    start: 70191,
    length: 3,
    convRule: rule92
  }, {
    start: 70194,
    length: 2,
    convRule: rule124
  }, {
    start: 70196,
    length: 1,
    convRule: rule92
  }, {
    start: 70197,
    length: 1,
    convRule: rule124
  }, {
    start: 70198,
    length: 2,
    convRule: rule92
  }, {
    start: 70200,
    length: 6,
    convRule: rule2
  }, {
    start: 70206,
    length: 1,
    convRule: rule92
  }, {
    start: 70272,
    length: 7,
    convRule: rule14
  }, {
    start: 70280,
    length: 1,
    convRule: rule14
  }, {
    start: 70282,
    length: 4,
    convRule: rule14
  }, {
    start: 70287,
    length: 15,
    convRule: rule14
  }, {
    start: 70303,
    length: 10,
    convRule: rule14
  }, {
    start: 70313,
    length: 1,
    convRule: rule2
  }, {
    start: 70320,
    length: 47,
    convRule: rule14
  }, {
    start: 70367,
    length: 1,
    convRule: rule92
  }, {
    start: 70368,
    length: 3,
    convRule: rule124
  }, {
    start: 70371,
    length: 8,
    convRule: rule92
  }, {
    start: 70384,
    length: 10,
    convRule: rule8
  }, {
    start: 70400,
    length: 2,
    convRule: rule92
  }, {
    start: 70402,
    length: 2,
    convRule: rule124
  }, {
    start: 70405,
    length: 8,
    convRule: rule14
  }, {
    start: 70415,
    length: 2,
    convRule: rule14
  }, {
    start: 70419,
    length: 22,
    convRule: rule14
  }, {
    start: 70442,
    length: 7,
    convRule: rule14
  }, {
    start: 70450,
    length: 2,
    convRule: rule14
  }, {
    start: 70453,
    length: 5,
    convRule: rule14
  }, {
    start: 70459,
    length: 2,
    convRule: rule92
  }, {
    start: 70461,
    length: 1,
    convRule: rule14
  }, {
    start: 70462,
    length: 2,
    convRule: rule124
  }, {
    start: 70464,
    length: 1,
    convRule: rule92
  }, {
    start: 70465,
    length: 4,
    convRule: rule124
  }, {
    start: 70471,
    length: 2,
    convRule: rule124
  }, {
    start: 70475,
    length: 3,
    convRule: rule124
  }, {
    start: 70480,
    length: 1,
    convRule: rule14
  }, {
    start: 70487,
    length: 1,
    convRule: rule124
  }, {
    start: 70493,
    length: 5,
    convRule: rule14
  }, {
    start: 70498,
    length: 2,
    convRule: rule124
  }, {
    start: 70502,
    length: 7,
    convRule: rule92
  }, {
    start: 70512,
    length: 5,
    convRule: rule92
  }, {
    start: 70656,
    length: 53,
    convRule: rule14
  }, {
    start: 70709,
    length: 3,
    convRule: rule124
  }, {
    start: 70712,
    length: 8,
    convRule: rule92
  }, {
    start: 70720,
    length: 2,
    convRule: rule124
  }, {
    start: 70722,
    length: 3,
    convRule: rule92
  }, {
    start: 70725,
    length: 1,
    convRule: rule124
  }, {
    start: 70726,
    length: 1,
    convRule: rule92
  }, {
    start: 70727,
    length: 4,
    convRule: rule14
  }, {
    start: 70731,
    length: 5,
    convRule: rule2
  }, {
    start: 70736,
    length: 10,
    convRule: rule8
  }, {
    start: 70746,
    length: 2,
    convRule: rule2
  }, {
    start: 70749,
    length: 1,
    convRule: rule2
  }, {
    start: 70750,
    length: 1,
    convRule: rule92
  }, {
    start: 70751,
    length: 3,
    convRule: rule14
  }, {
    start: 70784,
    length: 48,
    convRule: rule14
  }, {
    start: 70832,
    length: 3,
    convRule: rule124
  }, {
    start: 70835,
    length: 6,
    convRule: rule92
  }, {
    start: 70841,
    length: 1,
    convRule: rule124
  }, {
    start: 70842,
    length: 1,
    convRule: rule92
  }, {
    start: 70843,
    length: 4,
    convRule: rule124
  }, {
    start: 70847,
    length: 2,
    convRule: rule92
  }, {
    start: 70849,
    length: 1,
    convRule: rule124
  }, {
    start: 70850,
    length: 2,
    convRule: rule92
  }, {
    start: 70852,
    length: 2,
    convRule: rule14
  }, {
    start: 70854,
    length: 1,
    convRule: rule2
  }, {
    start: 70855,
    length: 1,
    convRule: rule14
  }, {
    start: 70864,
    length: 10,
    convRule: rule8
  }, {
    start: 71040,
    length: 47,
    convRule: rule14
  }, {
    start: 71087,
    length: 3,
    convRule: rule124
  }, {
    start: 71090,
    length: 4,
    convRule: rule92
  }, {
    start: 71096,
    length: 4,
    convRule: rule124
  }, {
    start: 71100,
    length: 2,
    convRule: rule92
  }, {
    start: 71102,
    length: 1,
    convRule: rule124
  }, {
    start: 71103,
    length: 2,
    convRule: rule92
  }, {
    start: 71105,
    length: 23,
    convRule: rule2
  }, {
    start: 71128,
    length: 4,
    convRule: rule14
  }, {
    start: 71132,
    length: 2,
    convRule: rule92
  }, {
    start: 71168,
    length: 48,
    convRule: rule14
  }, {
    start: 71216,
    length: 3,
    convRule: rule124
  }, {
    start: 71219,
    length: 8,
    convRule: rule92
  }, {
    start: 71227,
    length: 2,
    convRule: rule124
  }, {
    start: 71229,
    length: 1,
    convRule: rule92
  }, {
    start: 71230,
    length: 1,
    convRule: rule124
  }, {
    start: 71231,
    length: 2,
    convRule: rule92
  }, {
    start: 71233,
    length: 3,
    convRule: rule2
  }, {
    start: 71236,
    length: 1,
    convRule: rule14
  }, {
    start: 71248,
    length: 10,
    convRule: rule8
  }, {
    start: 71264,
    length: 13,
    convRule: rule2
  }, {
    start: 71296,
    length: 43,
    convRule: rule14
  }, {
    start: 71339,
    length: 1,
    convRule: rule92
  }, {
    start: 71340,
    length: 1,
    convRule: rule124
  }, {
    start: 71341,
    length: 1,
    convRule: rule92
  }, {
    start: 71342,
    length: 2,
    convRule: rule124
  }, {
    start: 71344,
    length: 6,
    convRule: rule92
  }, {
    start: 71350,
    length: 1,
    convRule: rule124
  }, {
    start: 71351,
    length: 1,
    convRule: rule92
  }, {
    start: 71352,
    length: 1,
    convRule: rule14
  }, {
    start: 71360,
    length: 10,
    convRule: rule8
  }, {
    start: 71424,
    length: 27,
    convRule: rule14
  }, {
    start: 71453,
    length: 3,
    convRule: rule92
  }, {
    start: 71456,
    length: 2,
    convRule: rule124
  }, {
    start: 71458,
    length: 4,
    convRule: rule92
  }, {
    start: 71462,
    length: 1,
    convRule: rule124
  }, {
    start: 71463,
    length: 5,
    convRule: rule92
  }, {
    start: 71472,
    length: 10,
    convRule: rule8
  }, {
    start: 71482,
    length: 2,
    convRule: rule17
  }, {
    start: 71484,
    length: 3,
    convRule: rule2
  }, {
    start: 71487,
    length: 1,
    convRule: rule13
  }, {
    start: 71680,
    length: 44,
    convRule: rule14
  }, {
    start: 71724,
    length: 3,
    convRule: rule124
  }, {
    start: 71727,
    length: 9,
    convRule: rule92
  }, {
    start: 71736,
    length: 1,
    convRule: rule124
  }, {
    start: 71737,
    length: 2,
    convRule: rule92
  }, {
    start: 71739,
    length: 1,
    convRule: rule2
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 71904,
    length: 10,
    convRule: rule8
  }, {
    start: 71914,
    length: 9,
    convRule: rule17
  }, {
    start: 71935,
    length: 8,
    convRule: rule14
  }, {
    start: 71945,
    length: 1,
    convRule: rule14
  }, {
    start: 71948,
    length: 8,
    convRule: rule14
  }, {
    start: 71957,
    length: 2,
    convRule: rule14
  }, {
    start: 71960,
    length: 24,
    convRule: rule14
  }, {
    start: 71984,
    length: 6,
    convRule: rule124
  }, {
    start: 71991,
    length: 2,
    convRule: rule124
  }, {
    start: 71995,
    length: 2,
    convRule: rule92
  }, {
    start: 71997,
    length: 1,
    convRule: rule124
  }, {
    start: 71998,
    length: 1,
    convRule: rule92
  }, {
    start: 71999,
    length: 1,
    convRule: rule14
  }, {
    start: 72e3,
    length: 1,
    convRule: rule124
  }, {
    start: 72001,
    length: 1,
    convRule: rule14
  }, {
    start: 72002,
    length: 1,
    convRule: rule124
  }, {
    start: 72003,
    length: 1,
    convRule: rule92
  }, {
    start: 72004,
    length: 3,
    convRule: rule2
  }, {
    start: 72016,
    length: 10,
    convRule: rule8
  }, {
    start: 72096,
    length: 8,
    convRule: rule14
  }, {
    start: 72106,
    length: 39,
    convRule: rule14
  }, {
    start: 72145,
    length: 3,
    convRule: rule124
  }, {
    start: 72148,
    length: 4,
    convRule: rule92
  }, {
    start: 72154,
    length: 2,
    convRule: rule92
  }, {
    start: 72156,
    length: 4,
    convRule: rule124
  }, {
    start: 72160,
    length: 1,
    convRule: rule92
  }, {
    start: 72161,
    length: 1,
    convRule: rule14
  }, {
    start: 72162,
    length: 1,
    convRule: rule2
  }, {
    start: 72163,
    length: 1,
    convRule: rule14
  }, {
    start: 72164,
    length: 1,
    convRule: rule124
  }, {
    start: 72192,
    length: 1,
    convRule: rule14
  }, {
    start: 72193,
    length: 10,
    convRule: rule92
  }, {
    start: 72203,
    length: 40,
    convRule: rule14
  }, {
    start: 72243,
    length: 6,
    convRule: rule92
  }, {
    start: 72249,
    length: 1,
    convRule: rule124
  }, {
    start: 72250,
    length: 1,
    convRule: rule14
  }, {
    start: 72251,
    length: 4,
    convRule: rule92
  }, {
    start: 72255,
    length: 8,
    convRule: rule2
  }, {
    start: 72263,
    length: 1,
    convRule: rule92
  }, {
    start: 72272,
    length: 1,
    convRule: rule14
  }, {
    start: 72273,
    length: 6,
    convRule: rule92
  }, {
    start: 72279,
    length: 2,
    convRule: rule124
  }, {
    start: 72281,
    length: 3,
    convRule: rule92
  }, {
    start: 72284,
    length: 46,
    convRule: rule14
  }, {
    start: 72330,
    length: 13,
    convRule: rule92
  }, {
    start: 72343,
    length: 1,
    convRule: rule124
  }, {
    start: 72344,
    length: 2,
    convRule: rule92
  }, {
    start: 72346,
    length: 3,
    convRule: rule2
  }, {
    start: 72349,
    length: 1,
    convRule: rule14
  }, {
    start: 72350,
    length: 5,
    convRule: rule2
  }, {
    start: 72384,
    length: 57,
    convRule: rule14
  }, {
    start: 72704,
    length: 9,
    convRule: rule14
  }, {
    start: 72714,
    length: 37,
    convRule: rule14
  }, {
    start: 72751,
    length: 1,
    convRule: rule124
  }, {
    start: 72752,
    length: 7,
    convRule: rule92
  }, {
    start: 72760,
    length: 6,
    convRule: rule92
  }, {
    start: 72766,
    length: 1,
    convRule: rule124
  }, {
    start: 72767,
    length: 1,
    convRule: rule92
  }, {
    start: 72768,
    length: 1,
    convRule: rule14
  }, {
    start: 72769,
    length: 5,
    convRule: rule2
  }, {
    start: 72784,
    length: 10,
    convRule: rule8
  }, {
    start: 72794,
    length: 19,
    convRule: rule17
  }, {
    start: 72816,
    length: 2,
    convRule: rule2
  }, {
    start: 72818,
    length: 30,
    convRule: rule14
  }, {
    start: 72850,
    length: 22,
    convRule: rule92
  }, {
    start: 72873,
    length: 1,
    convRule: rule124
  }, {
    start: 72874,
    length: 7,
    convRule: rule92
  }, {
    start: 72881,
    length: 1,
    convRule: rule124
  }, {
    start: 72882,
    length: 2,
    convRule: rule92
  }, {
    start: 72884,
    length: 1,
    convRule: rule124
  }, {
    start: 72885,
    length: 2,
    convRule: rule92
  }, {
    start: 72960,
    length: 7,
    convRule: rule14
  }, {
    start: 72968,
    length: 2,
    convRule: rule14
  }, {
    start: 72971,
    length: 38,
    convRule: rule14
  }, {
    start: 73009,
    length: 6,
    convRule: rule92
  }, {
    start: 73018,
    length: 1,
    convRule: rule92
  }, {
    start: 73020,
    length: 2,
    convRule: rule92
  }, {
    start: 73023,
    length: 7,
    convRule: rule92
  }, {
    start: 73030,
    length: 1,
    convRule: rule14
  }, {
    start: 73031,
    length: 1,
    convRule: rule92
  }, {
    start: 73040,
    length: 10,
    convRule: rule8
  }, {
    start: 73056,
    length: 6,
    convRule: rule14
  }, {
    start: 73063,
    length: 2,
    convRule: rule14
  }, {
    start: 73066,
    length: 32,
    convRule: rule14
  }, {
    start: 73098,
    length: 5,
    convRule: rule124
  }, {
    start: 73104,
    length: 2,
    convRule: rule92
  }, {
    start: 73107,
    length: 2,
    convRule: rule124
  }, {
    start: 73109,
    length: 1,
    convRule: rule92
  }, {
    start: 73110,
    length: 1,
    convRule: rule124
  }, {
    start: 73111,
    length: 1,
    convRule: rule92
  }, {
    start: 73112,
    length: 1,
    convRule: rule14
  }, {
    start: 73120,
    length: 10,
    convRule: rule8
  }, {
    start: 73440,
    length: 19,
    convRule: rule14
  }, {
    start: 73459,
    length: 2,
    convRule: rule92
  }, {
    start: 73461,
    length: 2,
    convRule: rule124
  }, {
    start: 73463,
    length: 2,
    convRule: rule2
  }, {
    start: 73648,
    length: 1,
    convRule: rule14
  }, {
    start: 73664,
    length: 21,
    convRule: rule17
  }, {
    start: 73685,
    length: 8,
    convRule: rule13
  }, {
    start: 73693,
    length: 4,
    convRule: rule3
  }, {
    start: 73697,
    length: 17,
    convRule: rule13
  }, {
    start: 73727,
    length: 1,
    convRule: rule2
  }, {
    start: 73728,
    length: 922,
    convRule: rule14
  }, {
    start: 74752,
    length: 111,
    convRule: rule128
  }, {
    start: 74864,
    length: 5,
    convRule: rule2
  }, {
    start: 74880,
    length: 196,
    convRule: rule14
  }, {
    start: 77824,
    length: 1071,
    convRule: rule14
  }, {
    start: 78896,
    length: 9,
    convRule: rule16
  }, {
    start: 82944,
    length: 583,
    convRule: rule14
  }, {
    start: 92160,
    length: 569,
    convRule: rule14
  }, {
    start: 92736,
    length: 31,
    convRule: rule14
  }, {
    start: 92768,
    length: 10,
    convRule: rule8
  }, {
    start: 92782,
    length: 2,
    convRule: rule2
  }, {
    start: 92880,
    length: 30,
    convRule: rule14
  }, {
    start: 92912,
    length: 5,
    convRule: rule92
  }, {
    start: 92917,
    length: 1,
    convRule: rule2
  }, {
    start: 92928,
    length: 48,
    convRule: rule14
  }, {
    start: 92976,
    length: 7,
    convRule: rule92
  }, {
    start: 92983,
    length: 5,
    convRule: rule2
  }, {
    start: 92988,
    length: 4,
    convRule: rule13
  }, {
    start: 92992,
    length: 4,
    convRule: rule91
  }, {
    start: 92996,
    length: 1,
    convRule: rule2
  }, {
    start: 92997,
    length: 1,
    convRule: rule13
  }, {
    start: 93008,
    length: 10,
    convRule: rule8
  }, {
    start: 93019,
    length: 7,
    convRule: rule17
  }, {
    start: 93027,
    length: 21,
    convRule: rule14
  }, {
    start: 93053,
    length: 19,
    convRule: rule14
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 93824,
    length: 23,
    convRule: rule17
  }, {
    start: 93847,
    length: 4,
    convRule: rule2
  }, {
    start: 93952,
    length: 75,
    convRule: rule14
  }, {
    start: 94031,
    length: 1,
    convRule: rule92
  }, {
    start: 94032,
    length: 1,
    convRule: rule14
  }, {
    start: 94033,
    length: 55,
    convRule: rule124
  }, {
    start: 94095,
    length: 4,
    convRule: rule92
  }, {
    start: 94099,
    length: 13,
    convRule: rule91
  }, {
    start: 94176,
    length: 2,
    convRule: rule91
  }, {
    start: 94178,
    length: 1,
    convRule: rule2
  }, {
    start: 94179,
    length: 1,
    convRule: rule91
  }, {
    start: 94180,
    length: 1,
    convRule: rule92
  }, {
    start: 94192,
    length: 2,
    convRule: rule124
  }, {
    start: 94208,
    length: 6136,
    convRule: rule14
  }, {
    start: 100352,
    length: 1238,
    convRule: rule14
  }, {
    start: 101632,
    length: 9,
    convRule: rule14
  }, {
    start: 110592,
    length: 287,
    convRule: rule14
  }, {
    start: 110928,
    length: 3,
    convRule: rule14
  }, {
    start: 110948,
    length: 4,
    convRule: rule14
  }, {
    start: 110960,
    length: 396,
    convRule: rule14
  }, {
    start: 113664,
    length: 107,
    convRule: rule14
  }, {
    start: 113776,
    length: 13,
    convRule: rule14
  }, {
    start: 113792,
    length: 9,
    convRule: rule14
  }, {
    start: 113808,
    length: 10,
    convRule: rule14
  }, {
    start: 113820,
    length: 1,
    convRule: rule13
  }, {
    start: 113821,
    length: 2,
    convRule: rule92
  }, {
    start: 113823,
    length: 1,
    convRule: rule2
  }, {
    start: 113824,
    length: 4,
    convRule: rule16
  }, {
    start: 118784,
    length: 246,
    convRule: rule13
  }, {
    start: 119040,
    length: 39,
    convRule: rule13
  }, {
    start: 119081,
    length: 60,
    convRule: rule13
  }, {
    start: 119141,
    length: 2,
    convRule: rule124
  }, {
    start: 119143,
    length: 3,
    convRule: rule92
  }, {
    start: 119146,
    length: 3,
    convRule: rule13
  }, {
    start: 119149,
    length: 6,
    convRule: rule124
  }, {
    start: 119155,
    length: 8,
    convRule: rule16
  }, {
    start: 119163,
    length: 8,
    convRule: rule92
  }, {
    start: 119171,
    length: 2,
    convRule: rule13
  }, {
    start: 119173,
    length: 7,
    convRule: rule92
  }, {
    start: 119180,
    length: 30,
    convRule: rule13
  }, {
    start: 119210,
    length: 4,
    convRule: rule92
  }, {
    start: 119214,
    length: 59,
    convRule: rule13
  }, {
    start: 119296,
    length: 66,
    convRule: rule13
  }, {
    start: 119362,
    length: 3,
    convRule: rule92
  }, {
    start: 119365,
    length: 1,
    convRule: rule13
  }, {
    start: 119520,
    length: 20,
    convRule: rule17
  }, {
    start: 119552,
    length: 87,
    convRule: rule13
  }, {
    start: 119648,
    length: 25,
    convRule: rule17
  }, {
    start: 119808,
    length: 26,
    convRule: rule107
  }, {
    start: 119834,
    length: 26,
    convRule: rule20
  }, {
    start: 119860,
    length: 26,
    convRule: rule107
  }, {
    start: 119886,
    length: 7,
    convRule: rule20
  }, {
    start: 119894,
    length: 18,
    convRule: rule20
  }, {
    start: 119912,
    length: 26,
    convRule: rule107
  }, {
    start: 119938,
    length: 26,
    convRule: rule20
  }, {
    start: 119964,
    length: 1,
    convRule: rule107
  }, {
    start: 119966,
    length: 2,
    convRule: rule107
  }, {
    start: 119970,
    length: 1,
    convRule: rule107
  }, {
    start: 119973,
    length: 2,
    convRule: rule107
  }, {
    start: 119977,
    length: 4,
    convRule: rule107
  }, {
    start: 119982,
    length: 8,
    convRule: rule107
  }, {
    start: 119990,
    length: 4,
    convRule: rule20
  }, {
    start: 119995,
    length: 1,
    convRule: rule20
  }, {
    start: 119997,
    length: 7,
    convRule: rule20
  }, {
    start: 120005,
    length: 11,
    convRule: rule20
  }, {
    start: 120016,
    length: 26,
    convRule: rule107
  }, {
    start: 120042,
    length: 26,
    convRule: rule20
  }, {
    start: 120068,
    length: 2,
    convRule: rule107
  }, {
    start: 120071,
    length: 4,
    convRule: rule107
  }, {
    start: 120077,
    length: 8,
    convRule: rule107
  }, {
    start: 120086,
    length: 7,
    convRule: rule107
  }, {
    start: 120094,
    length: 26,
    convRule: rule20
  }, {
    start: 120120,
    length: 2,
    convRule: rule107
  }, {
    start: 120123,
    length: 4,
    convRule: rule107
  }, {
    start: 120128,
    length: 5,
    convRule: rule107
  }, {
    start: 120134,
    length: 1,
    convRule: rule107
  }, {
    start: 120138,
    length: 7,
    convRule: rule107
  }, {
    start: 120146,
    length: 26,
    convRule: rule20
  }, {
    start: 120172,
    length: 26,
    convRule: rule107
  }, {
    start: 120198,
    length: 26,
    convRule: rule20
  }, {
    start: 120224,
    length: 26,
    convRule: rule107
  }, {
    start: 120250,
    length: 26,
    convRule: rule20
  }, {
    start: 120276,
    length: 26,
    convRule: rule107
  }, {
    start: 120302,
    length: 26,
    convRule: rule20
  }, {
    start: 120328,
    length: 26,
    convRule: rule107
  }, {
    start: 120354,
    length: 26,
    convRule: rule20
  }, {
    start: 120380,
    length: 26,
    convRule: rule107
  }, {
    start: 120406,
    length: 26,
    convRule: rule20
  }, {
    start: 120432,
    length: 26,
    convRule: rule107
  }, {
    start: 120458,
    length: 28,
    convRule: rule20
  }, {
    start: 120488,
    length: 25,
    convRule: rule107
  }, {
    start: 120513,
    length: 1,
    convRule: rule6
  }, {
    start: 120514,
    length: 25,
    convRule: rule20
  }, {
    start: 120539,
    length: 1,
    convRule: rule6
  }, {
    start: 120540,
    length: 6,
    convRule: rule20
  }, {
    start: 120546,
    length: 25,
    convRule: rule107
  }, {
    start: 120571,
    length: 1,
    convRule: rule6
  }, {
    start: 120572,
    length: 25,
    convRule: rule20
  }, {
    start: 120597,
    length: 1,
    convRule: rule6
  }, {
    start: 120598,
    length: 6,
    convRule: rule20
  }, {
    start: 120604,
    length: 25,
    convRule: rule107
  }, {
    start: 120629,
    length: 1,
    convRule: rule6
  }, {
    start: 120630,
    length: 25,
    convRule: rule20
  }, {
    start: 120655,
    length: 1,
    convRule: rule6
  }, {
    start: 120656,
    length: 6,
    convRule: rule20
  }, {
    start: 120662,
    length: 25,
    convRule: rule107
  }, {
    start: 120687,
    length: 1,
    convRule: rule6
  }, {
    start: 120688,
    length: 25,
    convRule: rule20
  }, {
    start: 120713,
    length: 1,
    convRule: rule6
  }, {
    start: 120714,
    length: 6,
    convRule: rule20
  }, {
    start: 120720,
    length: 25,
    convRule: rule107
  }, {
    start: 120745,
    length: 1,
    convRule: rule6
  }, {
    start: 120746,
    length: 25,
    convRule: rule20
  }, {
    start: 120771,
    length: 1,
    convRule: rule6
  }, {
    start: 120772,
    length: 6,
    convRule: rule20
  }, {
    start: 120778,
    length: 1,
    convRule: rule107
  }, {
    start: 120779,
    length: 1,
    convRule: rule20
  }, {
    start: 120782,
    length: 50,
    convRule: rule8
  }, {
    start: 120832,
    length: 512,
    convRule: rule13
  }, {
    start: 121344,
    length: 55,
    convRule: rule92
  }, {
    start: 121399,
    length: 4,
    convRule: rule13
  }, {
    start: 121403,
    length: 50,
    convRule: rule92
  }, {
    start: 121453,
    length: 8,
    convRule: rule13
  }, {
    start: 121461,
    length: 1,
    convRule: rule92
  }, {
    start: 121462,
    length: 14,
    convRule: rule13
  }, {
    start: 121476,
    length: 1,
    convRule: rule92
  }, {
    start: 121477,
    length: 2,
    convRule: rule13
  }, {
    start: 121479,
    length: 5,
    convRule: rule2
  }, {
    start: 121499,
    length: 5,
    convRule: rule92
  }, {
    start: 121505,
    length: 15,
    convRule: rule92
  }, {
    start: 122880,
    length: 7,
    convRule: rule92
  }, {
    start: 122888,
    length: 17,
    convRule: rule92
  }, {
    start: 122907,
    length: 7,
    convRule: rule92
  }, {
    start: 122915,
    length: 2,
    convRule: rule92
  }, {
    start: 122918,
    length: 5,
    convRule: rule92
  }, {
    start: 123136,
    length: 45,
    convRule: rule14
  }, {
    start: 123184,
    length: 7,
    convRule: rule92
  }, {
    start: 123191,
    length: 7,
    convRule: rule91
  }, {
    start: 123200,
    length: 10,
    convRule: rule8
  }, {
    start: 123214,
    length: 1,
    convRule: rule14
  }, {
    start: 123215,
    length: 1,
    convRule: rule13
  }, {
    start: 123584,
    length: 44,
    convRule: rule14
  }, {
    start: 123628,
    length: 4,
    convRule: rule92
  }, {
    start: 123632,
    length: 10,
    convRule: rule8
  }, {
    start: 123647,
    length: 1,
    convRule: rule3
  }, {
    start: 124928,
    length: 197,
    convRule: rule14
  }, {
    start: 125127,
    length: 9,
    convRule: rule17
  }, {
    start: 125136,
    length: 7,
    convRule: rule92
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }, {
    start: 125252,
    length: 7,
    convRule: rule92
  }, {
    start: 125259,
    length: 1,
    convRule: rule91
  }, {
    start: 125264,
    length: 10,
    convRule: rule8
  }, {
    start: 125278,
    length: 2,
    convRule: rule2
  }, {
    start: 126065,
    length: 59,
    convRule: rule17
  }, {
    start: 126124,
    length: 1,
    convRule: rule13
  }, {
    start: 126125,
    length: 3,
    convRule: rule17
  }, {
    start: 126128,
    length: 1,
    convRule: rule3
  }, {
    start: 126129,
    length: 4,
    convRule: rule17
  }, {
    start: 126209,
    length: 45,
    convRule: rule17
  }, {
    start: 126254,
    length: 1,
    convRule: rule13
  }, {
    start: 126255,
    length: 15,
    convRule: rule17
  }, {
    start: 126464,
    length: 4,
    convRule: rule14
  }, {
    start: 126469,
    length: 27,
    convRule: rule14
  }, {
    start: 126497,
    length: 2,
    convRule: rule14
  }, {
    start: 126500,
    length: 1,
    convRule: rule14
  }, {
    start: 126503,
    length: 1,
    convRule: rule14
  }, {
    start: 126505,
    length: 10,
    convRule: rule14
  }, {
    start: 126516,
    length: 4,
    convRule: rule14
  }, {
    start: 126521,
    length: 1,
    convRule: rule14
  }, {
    start: 126523,
    length: 1,
    convRule: rule14
  }, {
    start: 126530,
    length: 1,
    convRule: rule14
  }, {
    start: 126535,
    length: 1,
    convRule: rule14
  }, {
    start: 126537,
    length: 1,
    convRule: rule14
  }, {
    start: 126539,
    length: 1,
    convRule: rule14
  }, {
    start: 126541,
    length: 3,
    convRule: rule14
  }, {
    start: 126545,
    length: 2,
    convRule: rule14
  }, {
    start: 126548,
    length: 1,
    convRule: rule14
  }, {
    start: 126551,
    length: 1,
    convRule: rule14
  }, {
    start: 126553,
    length: 1,
    convRule: rule14
  }, {
    start: 126555,
    length: 1,
    convRule: rule14
  }, {
    start: 126557,
    length: 1,
    convRule: rule14
  }, {
    start: 126559,
    length: 1,
    convRule: rule14
  }, {
    start: 126561,
    length: 2,
    convRule: rule14
  }, {
    start: 126564,
    length: 1,
    convRule: rule14
  }, {
    start: 126567,
    length: 4,
    convRule: rule14
  }, {
    start: 126572,
    length: 7,
    convRule: rule14
  }, {
    start: 126580,
    length: 4,
    convRule: rule14
  }, {
    start: 126585,
    length: 4,
    convRule: rule14
  }, {
    start: 126590,
    length: 1,
    convRule: rule14
  }, {
    start: 126592,
    length: 10,
    convRule: rule14
  }, {
    start: 126603,
    length: 17,
    convRule: rule14
  }, {
    start: 126625,
    length: 3,
    convRule: rule14
  }, {
    start: 126629,
    length: 5,
    convRule: rule14
  }, {
    start: 126635,
    length: 17,
    convRule: rule14
  }, {
    start: 126704,
    length: 2,
    convRule: rule6
  }, {
    start: 126976,
    length: 44,
    convRule: rule13
  }, {
    start: 127024,
    length: 100,
    convRule: rule13
  }, {
    start: 127136,
    length: 15,
    convRule: rule13
  }, {
    start: 127153,
    length: 15,
    convRule: rule13
  }, {
    start: 127169,
    length: 15,
    convRule: rule13
  }, {
    start: 127185,
    length: 37,
    convRule: rule13
  }, {
    start: 127232,
    length: 13,
    convRule: rule17
  }, {
    start: 127245,
    length: 161,
    convRule: rule13
  }, {
    start: 127462,
    length: 29,
    convRule: rule13
  }, {
    start: 127504,
    length: 44,
    convRule: rule13
  }, {
    start: 127552,
    length: 9,
    convRule: rule13
  }, {
    start: 127568,
    length: 2,
    convRule: rule13
  }, {
    start: 127584,
    length: 6,
    convRule: rule13
  }, {
    start: 127744,
    length: 251,
    convRule: rule13
  }, {
    start: 127995,
    length: 5,
    convRule: rule10
  }, {
    start: 128e3,
    length: 728,
    convRule: rule13
  }, {
    start: 128736,
    length: 13,
    convRule: rule13
  }, {
    start: 128752,
    length: 13,
    convRule: rule13
  }, {
    start: 128768,
    length: 116,
    convRule: rule13
  }, {
    start: 128896,
    length: 89,
    convRule: rule13
  }, {
    start: 128992,
    length: 12,
    convRule: rule13
  }, {
    start: 129024,
    length: 12,
    convRule: rule13
  }, {
    start: 129040,
    length: 56,
    convRule: rule13
  }, {
    start: 129104,
    length: 10,
    convRule: rule13
  }, {
    start: 129120,
    length: 40,
    convRule: rule13
  }, {
    start: 129168,
    length: 30,
    convRule: rule13
  }, {
    start: 129200,
    length: 2,
    convRule: rule13
  }, {
    start: 129280,
    length: 121,
    convRule: rule13
  }, {
    start: 129402,
    length: 82,
    convRule: rule13
  }, {
    start: 129485,
    length: 135,
    convRule: rule13
  }, {
    start: 129632,
    length: 14,
    convRule: rule13
  }, {
    start: 129648,
    length: 5,
    convRule: rule13
  }, {
    start: 129656,
    length: 3,
    convRule: rule13
  }, {
    start: 129664,
    length: 7,
    convRule: rule13
  }, {
    start: 129680,
    length: 25,
    convRule: rule13
  }, {
    start: 129712,
    length: 7,
    convRule: rule13
  }, {
    start: 129728,
    length: 3,
    convRule: rule13
  }, {
    start: 129744,
    length: 7,
    convRule: rule13
  }, {
    start: 129792,
    length: 147,
    convRule: rule13
  }, {
    start: 129940,
    length: 55,
    convRule: rule13
  }, {
    start: 130032,
    length: 10,
    convRule: rule8
  }, {
    start: 131072,
    length: 42718,
    convRule: rule14
  }, {
    start: 173824,
    length: 4149,
    convRule: rule14
  }, {
    start: 177984,
    length: 222,
    convRule: rule14
  }, {
    start: 178208,
    length: 5762,
    convRule: rule14
  }, {
    start: 183984,
    length: 7473,
    convRule: rule14
  }, {
    start: 194560,
    length: 542,
    convRule: rule14
  }, {
    start: 196608,
    length: 4939,
    convRule: rule14
  }, {
    start: 917505,
    length: 1,
    convRule: rule16
  }, {
    start: 917536,
    length: 96,
    convRule: rule16
  }, {
    start: 917760,
    length: 240,
    convRule: rule92
  }, {
    start: 983040,
    length: 65534,
    convRule: rule200
  }, {
    start: 1048576,
    length: 65534,
    convRule: rule200
  }];
  var checkAttr = function(categories) {
    return function($$char2) {
      var numOfBlocks = function() {
        var $43 = $$char2 < 256;
        if ($43) {
          return numLat1Blocks;
        }
        ;
        return numBlocks;
      }();
      var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
  var uIswalpha = /* @__PURE__ */ checkAttr([gencatLL, gencatLU, gencatLT, gencatLM, gencatLO]);
  var uIswupper = /* @__PURE__ */ checkAttr([gencatLU, gencatLT]);

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var modify4 = unsafeCoerce2;
  var toLowerSimple = /* @__PURE__ */ modify4(uTowlower);
  var toUpperSimple = /* @__PURE__ */ modify4(uTowupper);
  var isUpper = function($66) {
    return uIswupper(fromEnum4($66));
  };
  var isSpace = function(c2) {
    var uc = fromEnum4(c2);
    var $28 = uc <= 823;
    if ($28) {
      return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
    }
    ;
    return uIswspace(uc);
  };
  var isOctDigit = function(c2) {
    var diff = fromEnum4(c2) - toCharCode2("0") | 0;
    return diff <= 7 && diff >= 0;
  };
  var isDecDigit = function(c2) {
    var diff = fromEnum4(c2) - toCharCode2("0") | 0;
    return diff <= 9 && diff >= 0;
  };
  var isHexDigit = function(c2) {
    return isDecDigit(c2) || (function() {
      var diff = fromEnum4(c2) - toCharCode2("A") | 0;
      return diff <= 5 && diff >= 0;
    }() || function() {
      var diff = fromEnum4(c2) - toCharCode2("a") | 0;
      return diff <= 5 && diff >= 0;
    }());
  };
  var isAlphaNum = function($70) {
    return uIswalnum(fromEnum4($70));
  };
  var isAlpha = function($71) {
    return uIswalpha(fromEnum4($71));
  };
  var hexDigitToInt = function(c2) {
    var hexUpper = fromEnum4(c2) - toCharCode2("A") | 0;
    var hexLower = fromEnum4(c2) - toCharCode2("a") | 0;
    var dec = fromEnum4(c2) - toCharCode2("0") | 0;
    var result = function() {
      if (dec <= 9 && dec >= 0) {
        return new Just(dec);
      }
      ;
      if (hexLower <= 5 && hexLower >= 0) {
        return new Just(hexLower + 10 | 0);
      }
      ;
      if (hexUpper <= 5 && hexUpper >= 0) {
        return new Just(hexUpper + 10 | 0);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + []);
    }();
    return result;
  };

  // output/Parsing.String.Basic/index.js
  var elem1 = /* @__PURE__ */ elem2(eqChar);
  var show12 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showChar));
  var notElem1 = /* @__PURE__ */ notElem2(eqChar);
  var satisfyCP = function(p5) {
    return satisfy(function($30) {
      return p5(codePointFromChar($30));
    });
  };
  var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
  var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
  var oneOf2 = function(ss) {
    return withLazyErrorMessage(satisfy(flip(elem1)(ss)))(function(v) {
      return "one of " + show12(ss);
    });
  };
  var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
  var noneOf = function(ss) {
    return withLazyErrorMessage(satisfy(flip(notElem1)(ss)))(function(v) {
      return "none of " + show12(ss);
    });
  };
  var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
  var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
  var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
  var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

  // output/Data.String.Unicode/index.js
  var map19 = /* @__PURE__ */ map(functorArray);
  var convert = function(f) {
    var $6 = map19(f);
    return function($7) {
      return fromCodePointArray($6(toCodePointArray($7)));
    };
  };
  var toLowerSimple2 = /* @__PURE__ */ convert(toLowerSimple);
  var toUpperSimple2 = /* @__PURE__ */ convert(toUpperSimple);

  // output/Parsing.Token/index.js
  var bind9 = /* @__PURE__ */ bind(bindParserT);
  var pure17 = /* @__PURE__ */ pure(applicativeParserT);
  var sort2 = /* @__PURE__ */ sort(ordString);
  var map20 = /* @__PURE__ */ map(functorArray);
  var applySecond4 = /* @__PURE__ */ applySecond(applyParserT);
  var compare3 = /* @__PURE__ */ compare(ordString);
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var fix5 = /* @__PURE__ */ fix(lazyParserT);
  var alt4 = /* @__PURE__ */ alt(altParserT);
  var $$void4 = /* @__PURE__ */ $$void(functorParserT);
  var voidLeft4 = /* @__PURE__ */ voidLeft(functorParserT);
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var many3 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var map110 = /* @__PURE__ */ map(functorMaybe);
  var some3 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
  var foldl5 = /* @__PURE__ */ foldl(foldableArray);
  var applyFirst3 = /* @__PURE__ */ applyFirst(applyParserT);
  var show5 = /* @__PURE__ */ show(showString);
  var bind12 = /* @__PURE__ */ bind(bindMaybe);
  var pure18 = /* @__PURE__ */ pure(applicativeMaybe);
  var foldr4 = /* @__PURE__ */ foldr(foldableArray);
  var map23 = /* @__PURE__ */ map(functorParserT);
  var choice3 = /* @__PURE__ */ choice(foldableArray);
  var many1 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var toUnfoldable7 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var foldr12 = /* @__PURE__ */ foldr(foldableList);
  var unGenLanguageDef = function(v) {
    return v;
  };
  var theReservedNames = function(v) {
    if (v.caseSensitive) {
      return sort2(v.reservedNames);
    }
    ;
    if (otherwise) {
      return sort2(map20(toLower)(v.reservedNames));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 825, column 1 - line 825, column 70): " + [v.constructor.name]);
  };
  var simpleSpace = /* @__PURE__ */ skipMany1(/* @__PURE__ */ satisfyCodePoint(isSpace));
  var oneLineComment = function(v) {
    return applySecond4($$try3(string(v.commentLine)))(skipMany(satisfy(function(v1) {
      return v1 !== "\n";
    })));
  };
  var isReserved = function($copy_names) {
    return function($copy_name) {
      var $tco_var_names = $copy_names;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(names, name2) {
        var v = uncons(names);
        if (v instanceof Nothing) {
          $tco_done = true;
          return false;
        }
        ;
        if (v instanceof Just) {
          var v1 = compare3(v.value0.head)(name2);
          if (v1 instanceof LT) {
            $tco_var_names = v.value0.tail;
            $copy_name = name2;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return true;
          }
          ;
          if (v1 instanceof GT) {
            $tco_done = true;
            return false;
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 820, column 35 - line 823, column 18): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 818, column 3 - line 823, column 18): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_names, $copy_name);
      }
      ;
      return $tco_result;
    };
  };
  var isReservedName = function(v) {
    return function(name2) {
      var caseName = function() {
        if (v.caseSensitive) {
          return name2;
        }
        ;
        if (otherwise) {
          return toLower(name2);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 812, column 3 - line 814, column 31): " + []);
      }();
      return isReserved(theReservedNames(v))(caseName);
    };
  };
  var inCommentSingle = function(v) {
    var startEnd = append7(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix5(function(p5) {
      return alt4($$void4($$try3(string(v.commentEnd))))(alt4(applySecond4(skipMany1(noneOf(startEnd)))(p5))(withErrorMessage(applySecond4(oneOf2(startEnd))(p5))("end of comment")));
    });
  };
  var multiLineComment = function(v) {
    return applySecond4($$try3(string(v.commentStart)))(inComment(v));
  };
  var inCommentMulti = function(v) {
    var startEnd = append7(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix5(function(p5) {
      return alt4($$void4($$try3(string(v.commentEnd))))(alt4(applySecond4(multiLineComment(v))(p5))(alt4(applySecond4(skipMany1(noneOf(startEnd)))(p5))(withErrorMessage(applySecond4(oneOf2(startEnd))(p5))("end of comment"))));
    });
  };
  var inComment = function(v) {
    if (v.nestedComments) {
      return inCommentMulti(v);
    }
    ;
    return inCommentSingle(v);
  };
  var whiteSpace$prime = function(v) {
    if ($$null2(v.commentLine) && $$null2(v.commentStart)) {
      return skipMany(withErrorMessage(simpleSpace)(""));
    }
    ;
    if ($$null2(v.commentLine)) {
      return skipMany(alt4(simpleSpace)(withErrorMessage(multiLineComment(v))("")));
    }
    ;
    if ($$null2(v.commentStart)) {
      return skipMany(alt4(simpleSpace)(withErrorMessage(oneLineComment(v))("")));
    }
    ;
    if (otherwise) {
      return skipMany(alt4(simpleSpace)(alt4(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 834, column 1 - line 834, column 74): " + [v.constructor.name]);
  };
  var makeTokenParser = function(v) {
    var stringLetter = satisfy(function(c2) {
      return c2 !== '"' && (c2 !== "\\" && c2 > "");
    });
    var sign2 = function(dictRing) {
      return alt4(voidLeft4($$char("-"))(negate(dictRing)))(alt4(voidLeft4($$char("+"))(identity9))(pure17(identity9)));
    };
    var sign1 = sign2(ringInt);
    var oper = function() {
      var go = bind9(v.opStart)(function(c2) {
        return bind9(many3(v.opLetter))(function(cs) {
          return pure17(singleton5(c2) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go)("operator");
    }();
    var number = function(base) {
      return function(baseDigit) {
        var folder = function(v1) {
          return function(v2) {
            if (v1 instanceof Nothing) {
              return Nothing.value;
            }
            ;
            if (v1 instanceof Just) {
              return map110(function(v3) {
                return (base * v1.value0 | 0) + v3 | 0;
              })(hexDigitToInt(codePointFromChar(v2)));
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 704, column 5 - line 704, column 45): " + [v1.constructor.name, v2.constructor.name]);
          };
        };
        return bind9(some3(baseDigit))(function(digits) {
          return maybe(fail("not digits"))(pure17)(foldl5(folder)(new Just(0))(digits));
        });
      };
    };
    var octal = applySecond4(oneOf2(["o", "O"]))(number(8)(octDigit));
    var lexeme = function(p5) {
      return applyFirst3(p5)(whiteSpace$prime(v));
    };
    var reservedOp = function(name2) {
      var go = bind9(string(name2))(function() {
        return withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2);
      });
      return lexeme($$try3(go));
    };
    var symbol2 = function(name2) {
      return voidLeft4(lexeme(string(name2)))(name2);
    };
    var parens4 = function(p5) {
      return between(symbol2("("))(symbol2(")"))(p5);
    };
    var semi = symbol2(";");
    var semiSep = function(p5) {
      return sepBy(p5)(semi);
    };
    var semiSep1 = function(p5) {
      return sepBy1(p5)(semi);
    };
    var isReservedOp = function(name2) {
      return isReserved(sort2(v.reservedOpNames))(name2);
    };
    var operator = function() {
      var go = bind9(oper)(function(name2) {
        var $113 = isReservedOp(name2);
        if ($113) {
          return fail("reserved operator " + name2);
        }
        ;
        return pure17(name2);
      });
      return lexeme($$try3(go));
    }();
    var ident = function() {
      var go = bind9(v.identStart)(function(c2) {
        return bind9(many3(v.identLetter))(function(cs) {
          return pure17(singleton5(c2) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go)("identifier");
    }();
    var identifier2 = function() {
      var go = bind9(ident)(function(name2) {
        var $114 = isReservedName(v)(name2);
        if ($114) {
          return fail("reserved word " + show5(name2));
        }
        ;
        return pure17(name2);
      });
      return lexeme($$try3(go));
    }();
    var hexadecimal2 = applySecond4(oneOf2(["x", "X"]))(number(16)(hexDigit));
    var fraction = function() {
      var op2 = function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          if (v2 instanceof Just) {
            return bind12(hexDigitToInt(codePointFromChar(v1)))(function(int$prime) {
              return pure18((v2.value0 + toNumber(int$prime)) / 10);
            });
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 651, column 5 - line 651, column 47): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      return asErrorMessage("fraction")(bind9($$char("."))(function() {
        return bind9(withErrorMessage(some3(digit))("fraction"))(function(digits) {
          return maybe(fail("not digit"))(pure17)(foldr4(op2)(new Just(0))(digits));
        });
      }));
    }();
    var escapeGap = withErrorMessage(applySecond4(some3(space))($$char("\\")))("end of string gap");
    var escapeEmpty = $$char("&");
    var escMap = zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"]);
    var dot = symbol2(".");
    var decimal = number(10)(digit);
    var exponent$prime = function() {
      var power = function(e) {
        if (e < 0) {
          return 1 / power(-e | 0);
        }
        ;
        if (otherwise) {
          return pow(10)(toNumber(e));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 664, column 5 - line 664, column 27): " + [e.constructor.name]);
      };
      return asErrorMessage("exponent")(bind9(oneOf2(["e", "E"]))(function() {
        return bind9(sign1)(function(f) {
          return bind9(withErrorMessage(decimal)("exponent"))(function(e) {
            return pure17(power(f(e)));
          });
        });
      }));
    }();
    var fractExponent = function(n) {
      var justExponent = bind9(exponent$prime)(function(expo) {
        return pure17(toNumber(n) * expo);
      });
      var fractExponent$prime = bind9(fraction)(function(fract) {
        return bind9(option(1)(exponent$prime))(function(expo) {
          return pure17((toNumber(n) + fract) * expo);
        });
      });
      return alt4(fractExponent$prime)(justExponent);
    };
    var fractFloat = function(n) {
      return map23(Right.create)(fractExponent(n));
    };
    var decimalFloat = bind9(decimal)(function(n) {
      return option(new Left(n))(fractFloat(n));
    });
    var zeroNumFloat = alt4(map23(Left.create)(alt4(hexadecimal2)(octal)))(alt4(decimalFloat)(alt4(fractFloat(0))(pure17(new Left(0)))));
    var natFloat = alt4(applySecond4($$char("0"))(zeroNumFloat))(decimalFloat);
    var naturalOrFloat = withErrorMessage(lexeme(natFloat))("number");
    var floating = bind9(decimal)(fractExponent);
    var $$float = withErrorMessage(lexeme(floating))("float");
    var zeroNumber = withErrorMessage(applySecond4($$char("0"))(alt4(hexadecimal2)(alt4(octal)(alt4(decimal)(pure17(0))))))("");
    var nat = alt4(zeroNumber)(decimal);
    var $$int = bind9(lexeme(sign1))(function(f) {
      return bind9(nat)(function(n) {
        return pure17(f(n));
      });
    });
    var integer = withErrorMessage(lexeme($$int))("integer");
    var natural = withErrorMessage(lexeme(nat))("natural");
    var comma2 = symbol2(",");
    var commaSep = function(p5) {
      return sepBy(p5)(comma2);
    };
    var commaSep1 = function(p5) {
      return sepBy1(p5)(comma2);
    };
    var colon = symbol2(":");
    var charNum = bind9(alt4(decimal)(alt4(applySecond4($$char("o"))(number(8)(octDigit)))(applySecond4($$char("x"))(number(16)(hexDigit)))))(function(code) {
      var $119 = code > 1114111;
      if ($119) {
        return fail("invalid escape sequence");
      }
      ;
      var v1 = fromCharCode3(code);
      if (v1 instanceof Just) {
        return pure17(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return fail("invalid character code (should not happen)");
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 498, column 10 - line 500, column 67): " + [v1.constructor.name]);
    });
    var charLetter = satisfy(function(c2) {
      return c2 !== "'" && (c2 !== "\\" && c2 > "");
    });
    var charEsc = function() {
      var parseEsc = function(v1) {
        return voidLeft4($$char(v1.value0))(v1.value1);
      };
      return choice3(map20(parseEsc)(escMap));
    }();
    var charControl = bind9($$char("^"))(function() {
      return bind9(upper2)(function(code) {
        var v1 = fromCharCode3((toCharCode2(code) - toCharCode2("A") | 0) + 1 | 0);
        if (v1 instanceof Just) {
          return pure17(v1.value0);
        }
        ;
        if (v1 instanceof Nothing) {
          return fail("invalid character code (should not happen)");
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 488, column 5 - line 490, column 67): " + [v1.constructor.name]);
      });
    });
    var caseString = function(name2) {
      if (v.caseSensitive) {
        return voidLeft4(string(name2))(name2);
      }
      ;
      if (otherwise) {
        var msg = show5(name2);
        var caseChar = function(c2) {
          var v1 = function(v2) {
            if (otherwise) {
              return $$char(c2);
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 355, column 1 - line 355, column 80): " + [c2.constructor.name]);
          };
          var $130 = isAlpha(codePointFromChar(c2));
          if ($130) {
            var $131 = toChar(toLowerSimple2(singleton5(c2)));
            if ($131 instanceof Just) {
              var $132 = toChar(toUpperSimple2(singleton5(c2)));
              if ($132 instanceof Just) {
                return alt4($$char($131.value0))($$char($132.value0));
              }
              ;
              return v1(true);
            }
            ;
            return v1(true);
          }
          ;
          return v1(true);
        };
        var walk = function(name$prime) {
          var v1 = uncons3(name$prime);
          if (v1 instanceof Nothing) {
            return pure17(unit);
          }
          ;
          if (v1 instanceof Just) {
            return applySecond4(withErrorMessage(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 757, column 22 - line 759, column 72): " + [v1.constructor.name]);
        };
        return voidLeft4(walk(name2))(name2);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 751, column 3 - line 751, column 50): " + [name2.constructor.name]);
    };
    var reserved = function(name2) {
      var go = applySecond4(caseString(name2))(withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2));
      return lexeme($$try3(go));
    };
    var brackets2 = function(p5) {
      return between(symbol2("["))(symbol2("]"))(p5);
    };
    var braces2 = function(p5) {
      return between(symbol2("{"))(symbol2("}"))(p5);
    };
    var ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"];
    var ascii3 = ["\0", "", "", "", "", "", "", "\x07", "", "", "", "", "", "", "", "", "", "", "\x1B", "\x7F"];
    var ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"];
    var ascii2 = ["\b", "	", "\n", "\v", "\f", "\r", "", "", "", "", "", "", "", " "];
    var asciiMap = zip(append7(ascii3codes)(ascii2codes))(append7(ascii3)(ascii2));
    var charAscii = function() {
      var parseAscii = function(v1) {
        return $$try3(voidLeft4(string(v1.value0))(v1.value1));
      };
      return choice3(map20(parseAscii)(asciiMap));
    }();
    var escapeCode = alt4(charEsc)(alt4(charNum)(alt4(charAscii)(withErrorMessage(charControl)("escape code"))));
    var charEscape = applySecond4($$char("\\"))(escapeCode);
    var characterChar = alt4(charLetter)(withErrorMessage(charEscape)("literal character"));
    var charLiteral = function() {
      var go = between($$char("'"))(withErrorMessage($$char("'"))("end of character"))(characterChar);
      return withErrorMessage(lexeme(go))("character");
    }();
    var stringEscape = bind9($$char("\\"))(function() {
      return alt4(voidLeft4(escapeGap)(Nothing.value))(alt4(voidLeft4(escapeEmpty)(Nothing.value))(map23(Just.create)(escapeCode)));
    });
    var stringChar = alt4(map23(Just.create)(stringLetter))(withErrorMessage(stringEscape)("string character"));
    var stringLiteral = function() {
      var folder = function(v1) {
        return function(chars) {
          if (v1 instanceof Nothing) {
            return chars;
          }
          ;
          if (v1 instanceof Just) {
            return new Cons(v1.value0, chars);
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 455, column 5 - line 455, column 51): " + [v1.constructor.name, chars.constructor.name]);
        };
      };
      var go = bind9(between($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many1(stringChar)))(function(maybeChars) {
        return pure17(fromCharArray(toUnfoldable7(foldr12(folder)(Nil.value)(maybeChars))));
      });
      return lexeme(withErrorMessage(go)("literal string"));
    }();
    var angles = function(p5) {
      return between(symbol2("<"))(symbol2(">"))(p5);
    };
    return {
      identifier: identifier2,
      reserved,
      operator,
      reservedOp,
      charLiteral,
      stringLiteral,
      natural,
      integer,
      "float": $$float,
      naturalOrFloat,
      decimal,
      hexadecimal: hexadecimal2,
      octal,
      symbol: symbol2,
      lexeme,
      whiteSpace: whiteSpace$prime(v),
      parens: parens4,
      braces: braces2,
      angles,
      brackets: brackets2,
      semi,
      comma: comma2,
      colon,
      dot,
      semiSep,
      semiSep1,
      commaSep,
      commaSep1
    };
  };

  // output/Parsing.Language/index.js
  var alt5 = /* @__PURE__ */ alt(altParserT);
  var emptyDef = /* @__PURE__ */ function() {
    var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    return {
      commentStart: "",
      commentEnd: "",
      commentLine: "",
      nestedComments: true,
      identStart: alt5(letter)($$char("_")),
      identLetter: alt5(alphaNum)(oneOf2(["_", "'"])),
      opStart: op$prime,
      opLetter: op$prime,
      reservedOpNames: [],
      reservedNames: [],
      caseSensitive: true
    };
  }();
  var haskellStyle = /* @__PURE__ */ function() {
    var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    var v = unGenLanguageDef(emptyDef);
    return {
      commentStart: "{-",
      commentEnd: "-}",
      commentLine: "--",
      nestedComments: true,
      identStart: letter,
      identLetter: alt5(alphaNum)(oneOf2(["_", "'"])),
      opStart: op$prime,
      opLetter: op$prime,
      reservedNames: [],
      reservedOpNames: [],
      caseSensitive: true
    };
  }();
  var haskell98Def = /* @__PURE__ */ function() {
    var v = unGenLanguageDef(haskellStyle);
    return {
      commentStart: v.commentStart,
      commentEnd: v.commentEnd,
      commentLine: v.commentLine,
      nestedComments: v.nestedComments,
      identStart: v.identStart,
      identLetter: v.identLetter,
      opStart: v.opStart,
      opLetter: v.opLetter,
      reservedNames: ["let", "in", "case", "of", "if", "then", "else", "data", "type", "class", "default", "deriving", "do", "import", "infix", "infixl", "infixr", "instance", "module", "newtype", "where", "primitive"],
      reservedOpNames: ["::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>"],
      caseSensitive: v.caseSensitive
    };
  }();
  var haskellDef = /* @__PURE__ */ function() {
    return {
      commentStart: haskell98Def.commentStart,
      commentEnd: haskell98Def.commentEnd,
      commentLine: haskell98Def.commentLine,
      nestedComments: haskell98Def.nestedComments,
      identStart: haskell98Def.identStart,
      identLetter: alt5(haskell98Def.identLetter)($$char("#")),
      opStart: haskell98Def.opStart,
      opLetter: haskell98Def.opLetter,
      reservedNames: append(semigroupArray)(haskell98Def.reservedNames)(["foreign", "import", "export", "primitive", "_ccall_", "_casm_", "forall"]),
      reservedOpNames: haskell98Def.reservedOpNames,
      caseSensitive: haskell98Def.caseSensitive
    };
  }();

  // output/TDParseTy/index.js
  var compose2 = /* @__PURE__ */ compose(semigroupoidFn);
  var pure19 = /* @__PURE__ */ pure(applicativeParserT);
  var applySecond5 = /* @__PURE__ */ applySecond(applyParserT);
  var choice4 = /* @__PURE__ */ choice(foldableArray);
  var map21 = /* @__PURE__ */ map(functorArray);
  var show6 = /* @__PURE__ */ show(showCat);
  var voidLeft5 = /* @__PURE__ */ voidLeft(functorParserT);
  var apply2 = /* @__PURE__ */ apply(applyArray);
  var lift23 = /* @__PURE__ */ lift2(applyArray);
  var pure110 = /* @__PURE__ */ pure(applicativeArray);
  var alt6 = /* @__PURE__ */ alt(altParserT);
  var fix6 = /* @__PURE__ */ fix(lazyParserT);
  var bind10 = /* @__PURE__ */ bind(bindParserT);
  var discard3 = /* @__PURE__ */ discard(discardUnit)(bindParserT);
  var $$void5 = /* @__PURE__ */ $$void(functorParserT);
  var tokenParser = /* @__PURE__ */ makeTokenParser(haskellDef);
  var whiteSpace = /* @__PURE__ */ function() {
    return tokenParser.whiteSpace;
  }();
  var symbol = /* @__PURE__ */ function() {
    return tokenParser.symbol;
  }();
  var prefix = function(p5) {
    return Prefix.create(chainl1(p5)(pure19(compose2)));
  };
  var parens2 = /* @__PURE__ */ function() {
    return tokenParser.parens;
  }();
  var mkOp = function(name2) {
    return function(op2) {
      return applySecond5(symbol(name2))(pure19(op2));
    };
  };
  var identifier = /* @__PURE__ */ function() {
    return tokenParser.identifier;
  }();
  var effConsD = /* @__PURE__ */ function() {
    return choice4([mkOp("C")(effC(T.value)(T.value)), mkOp("D")(effD(G.value)(G.value)), mkOp("S")(effS), mkOp("W")(effW(E.value)), mkOp("R")(effR(E.value))]);
  }();
  var comma = /* @__PURE__ */ function() {
    return tokenParser.comma;
  }();
  var cats = /* @__PURE__ */ map21(function(c2) {
    return new Tuple(show6(c2), c2);
  })(/* @__PURE__ */ enumFromTo(enumCat)(unfoldable1Array)(/* @__PURE__ */ bottom(boundedCat))(/* @__PURE__ */ top(boundedCat)));
  var catParser = /* @__PURE__ */ choice4(/* @__PURE__ */ map21(function(v) {
    return voidLeft5(symbol(v.value0))(v.value1);
  })(cats));
  var binary = function(name2) {
    return function(op2) {
      return function(assoc) {
        return new Infix(mkOp(name2)(op2), assoc);
      };
    };
  };
  var tableD = /* @__PURE__ */ function() {
    return [[prefix(effConsD)], [binary("->")(Arr.create)(AssocRight.value)]];
  }();
  var ats = /* @__PURE__ */ fromFoldable(foldableList)(atomicTypes);
  var effCons = /* @__PURE__ */ choice4([/* @__PURE__ */ mkOp("C")(function(ts) {
    return apply2(lift23(effC)(ats)(ats))(ts);
  }), /* @__PURE__ */ mkOp("D")(function(ts) {
    return apply2(pure110(effD(G.value)(G.value)))(ts);
  }), /* @__PURE__ */ mkOp("S")(function(ts) {
    return apply2(pure110(effS))(ts);
  }), /* @__PURE__ */ mkOp("W")(function(ts) {
    return apply2(map21(effW)(ats))(ts);
  }), /* @__PURE__ */ mkOp("R")(function(ts) {
    return apply2(map21(effR)(ats))(ts);
  })]);
  var table = /* @__PURE__ */ function() {
    return [[prefix(effCons)], [binary("->")(lift23(Arr.create))(AssocRight.value)]];
  }();
  var atomD = /* @__PURE__ */ function() {
    return choice4([mkOp("e")(E.value), mkOp("E")(E.value), mkOp("t")(T.value), mkOp("T")(T.value)]);
  }();
  var tyExpD = function(p5) {
    return buildExprParser(tableD)(alt6(atomD)(alt6(parens2(p5))(fail("Unrecognized type"))));
  };
  var tyParserD = /* @__PURE__ */ applySecond5(whiteSpace)(/* @__PURE__ */ fix6(tyExpD));
  var lexParser = function(dictApplicative) {
    var pure23 = pure(dictApplicative);
    return parens2(bind10(identifier)(function(s) {
      return discard3($$void5(comma))(function() {
        return bind10(alt6(catParser)(fail("Unrecognized category")))(function(c2) {
          return discard3($$void5(comma))(function() {
            return bind10(alt6(tyParserD)(fail("Unrecognized type")))(function(t) {
              return pure19(new Tuple(s, pure23(new Tuple(make_var(s + "'"), new Tuple(c2, t)))));
            });
          });
        });
      });
    }));
  };
  var lexParse = function(dictApplicative) {
    var lexParser1 = lexParser(dictApplicative);
    return function(w) {
      var v = runParser(w)(lexParser1);
      if (v instanceof Left) {
        return new Left(parseErrorMessage(v.value0));
      }
      ;
      if (v instanceof Right) {
        return new Right(v.value0);
      }
      ;
      throw new Error("Failed pattern match at TDParseTy (line 90, column 14 - line 92, column 21): " + [v.constructor.name]);
    };
  };
  var atom = function(dictApplicative) {
    var pure23 = pure(dictApplicative);
    return choice4([mkOp("e")(pure23(E.value)), mkOp("E")(pure23(E.value)), mkOp("t")(pure23(T.value)), mkOp("T")(pure23(T.value)), mkOp("g")(pure23(G.value)), mkOp("G")(pure23(G.value))]);
  };
  var atom1 = /* @__PURE__ */ atom(applicativeArray);
  var tyExp = function(p5) {
    return buildExprParser(table)(alt6(atom1)(alt6(parens2(p5))(fail("Unrecognized type"))));
  };
  var tyParser = /* @__PURE__ */ applySecond5(whiteSpace)(/* @__PURE__ */ fix6(tyExp));
  var tyParse = function(t) {
    return runParser(t)(tyParser);
  };

  // output/Data.Renderable/index.js
  var width = function(dict) {
    return dict.width;
  };
  var space2 = function(dict) {
    return dict.space;
  };
  var renderableString = {
    space: " ",
    newline: "\n",
    width: length4,
    Monoid0: function() {
      return monoidString;
    }
  };
  var newline = function(dict) {
    return dict.newline;
  };

  // output/Text.Pretty/index.js
  var crashWith4 = /* @__PURE__ */ crashWith();
  var fold3 = /* @__PURE__ */ fold(foldableArray);
  var replicate3 = /* @__PURE__ */ replicate2(unfoldableArray);
  var SFail = /* @__PURE__ */ function() {
    function SFail2() {
    }
    ;
    SFail2.value = new SFail2();
    return SFail2;
  }();
  var SEmpty = /* @__PURE__ */ function() {
    function SEmpty2() {
    }
    ;
    SEmpty2.value = new SEmpty2();
    return SEmpty2;
  }();
  var SText = /* @__PURE__ */ function() {
    function SText2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    SText2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new SText2(value0, value1, value2);
        };
      };
    };
    return SText2;
  }();
  var SLine = /* @__PURE__ */ function() {
    function SLine2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SLine2.create = function(value0) {
      return function(value1) {
        return new SLine2(value0, value1);
      };
    };
    return SLine2;
  }();
  var Empty = /* @__PURE__ */ function() {
    function Empty2() {
    }
    ;
    Empty2.value = new Empty2();
    return Empty2;
  }();
  var Fail = /* @__PURE__ */ function() {
    function Fail2() {
    }
    ;
    Fail2.value = new Fail2();
    return Fail2;
  }();
  var Cat = /* @__PURE__ */ function() {
    function Cat2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cat2.create = function(value0) {
      return function(value1) {
        return new Cat2(value0, value1);
      };
    };
    return Cat2;
  }();
  var Nest = /* @__PURE__ */ function() {
    function Nest2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Nest2.create = function(value0) {
      return function(value1) {
        return new Nest2(value0, value1);
      };
    };
    return Nest2;
  }();
  var Text = /* @__PURE__ */ function() {
    function Text2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Text2.create = function(value0) {
      return function(value1) {
        return new Text2(value0, value1);
      };
    };
    return Text2;
  }();
  var Line = /* @__PURE__ */ function() {
    function Line2() {
    }
    ;
    Line2.value = new Line2();
    return Line2;
  }();
  var FlatAlt = /* @__PURE__ */ function() {
    function FlatAlt2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    FlatAlt2.create = function(value0) {
      return function(value1) {
        return new FlatAlt2(value0, value1);
      };
    };
    return FlatAlt2;
  }();
  var Union = /* @__PURE__ */ function() {
    function Union2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Union2.create = function(value0) {
      return function(value1) {
        return new Union2(value0, value1);
      };
    };
    return Union2;
  }();
  var Column = /* @__PURE__ */ function() {
    function Column2(value0) {
      this.value0 = value0;
    }
    ;
    Column2.create = function(value0) {
      return new Column2(value0);
    };
    return Column2;
  }();
  var Nesting = /* @__PURE__ */ function() {
    function Nesting2(value0) {
      this.value0 = value0;
    }
    ;
    Nesting2.create = function(value0) {
      return new Nesting2(value0);
    };
    return Nesting2;
  }();
  var semigroupDoc = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Empty) {
          return v1;
        }
        ;
        if (v1 instanceof Empty) {
          return v;
        }
        ;
        return new Cat(v, v1);
      };
    }
  };
  var monoidDoc = /* @__PURE__ */ function() {
    return {
      mempty: Empty.value,
      Semigroup0: function() {
        return semigroupDoc;
      }
    };
  }();
  var mempty2 = /* @__PURE__ */ mempty(monoidDoc);
  var text2 = function(dictRenderable) {
    var width2 = width(dictRenderable);
    return function(a3) {
      var l2 = width2(a3);
      var $126 = l2 > 0;
      if ($126) {
        return new Text(l2, a3);
      }
      ;
      return mempty2;
    };
  };
  var fits = function($copy_dictRenderable) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictRenderable = $copy_dictRenderable;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictRenderable, v, v1) {
          if (v < 0) {
            $tco_done = true;
            return false;
          }
          ;
          if (v1 instanceof SFail) {
            $tco_done = true;
            return false;
          }
          ;
          if (v1 instanceof SEmpty) {
            $tco_done = true;
            return true;
          }
          ;
          if (v1 instanceof SText) {
            $tco_var_dictRenderable = dictRenderable;
            $tco_var_v = v - v1.value0 | 0;
            $copy_v1 = v1.value2;
            return;
          }
          ;
          if (v1 instanceof SLine) {
            $tco_done = true;
            return true;
          }
          ;
          throw new Error("Failed pattern match at Text.Pretty (line 389, column 1 - line 389, column 70): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictRenderable, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var error3 = function(msg) {
    return crashWith4(msg);
  };
  var copy = function(dictMonoid) {
    var fold12 = fold3(dictMonoid);
    return function(n) {
      return function(a3) {
        return fold12(replicate3(n)(a3));
      };
    };
  };
  var spaces = function(dictRenderable) {
    var Monoid0 = dictRenderable.Monoid0();
    var copy1 = copy(Monoid0);
    var space1 = space2(dictRenderable);
    var mempty1 = mempty(Monoid0);
    return function(n) {
      var $181 = n > 0;
      if ($181) {
        return copy1(n)(space1);
      }
      ;
      return mempty1;
    };
  };
  var layout = function(dictRenderable) {
    var Monoid0 = dictRenderable.Monoid0();
    var mempty1 = mempty(Monoid0);
    var append16 = append(Monoid0.Semigroup0());
    var newline2 = newline(dictRenderable);
    var spaces1 = spaces(dictRenderable);
    return function(v) {
      if (v instanceof SFail) {
        return error3("attempt to layout SFail");
      }
      ;
      if (v instanceof SEmpty) {
        return mempty1;
      }
      ;
      if (v instanceof SText) {
        return append16(v.value1)(layout(dictRenderable)(v.value2));
      }
      ;
      if (v instanceof SLine) {
        return append16(newline2)(append16(spaces1(v.value0))(layout(dictRenderable)(v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Text.Pretty (line 350, column 1 - line 350, column 59): " + [v.constructor.name]);
    };
  };
  var better = function(dictRenderable) {
    var fits1 = fits(dictRenderable);
    return function(w) {
      return function(k2) {
        return function(x2) {
          return function(y) {
            var $188 = fits1(w - k2 | 0)(x2);
            if ($188) {
              return x2;
            }
            ;
            return y;
          };
        };
      };
    };
  };
  var best = function(dictRenderable) {
    var better1 = better(dictRenderable);
    return function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nil) {
            return SEmpty.value;
          }
          ;
          if (v2 instanceof Cons) {
            if (v2.value0.value1 instanceof Empty) {
              return best(dictRenderable)(v)(v1)(v2.value1);
            }
            ;
            if (v2.value0.value1 instanceof Fail) {
              return SFail.value;
            }
            ;
            if (v2.value0.value1 instanceof Text) {
              return new SText(v2.value0.value1.value0, v2.value0.value1.value1, best(dictRenderable)(v)(v1 + v2.value0.value1.value0 | 0)(v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof Line) {
              return new SLine(v2.value0.value0, best(dictRenderable)(v)(v2.value0.value0)(v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof Cat) {
              return best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value0), new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value1), v2.value1)));
            }
            ;
            if (v2.value0.value1 instanceof Nest) {
              return best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0 + v2.value0.value1.value0 | 0, v2.value0.value1.value1), v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof Column) {
              return best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value0(v1)), v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof Nesting) {
              return best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value0(v2.value0.value0)), v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof FlatAlt) {
              return best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value0), v2.value1));
            }
            ;
            if (v2.value0.value1 instanceof Union) {
              return better1(v)(v1)(best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value0), v2.value1)))(best(dictRenderable)(v)(v1)(new Cons(new Tuple(v2.value0.value0, v2.value0.value1.value1), v2.value1)));
            }
            ;
            throw new Error("Failed pattern match at Text.Pretty (line 366, column 31 - line 380, column 42): " + [v2.value0.value1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Text.Pretty (line 358, column 1 - line 364, column 25): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    };
  };
  var render2 = function(dictRenderable) {
    var layout1 = layout(dictRenderable);
    var best1 = best(dictRenderable);
    return function(w) {
      return function(x2) {
        return layout1(best1(w)(0)(new Cons(new Tuple(0, x2), Nil.value)));
      };
    };
  };

  // output/TDPretty/index.js
  var render3 = /* @__PURE__ */ render2(renderableString);
  var text3 = /* @__PURE__ */ text2(renderableString);
  var map24 = /* @__PURE__ */ map(functorList);
  var intercalate5 = /* @__PURE__ */ intercalate2(foldableList)(monoidString);
  var show7 = /* @__PURE__ */ show(showOp);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var arrayToNodeData2 = /* @__PURE__ */ arrayToNodeData(nodeDataToNodedata);
  var arrayToNodeData1 = /* @__PURE__ */ arrayToNodeData(htmlToHtml);
  var span4 = /* @__PURE__ */ span(arrayToNodeData2)(arrayToNodeData1);
  var class$prime2 = /* @__PURE__ */ class$prime(stringClassList);
  var $$null6 = /* @__PURE__ */ $$null(foldableArray);
  var eq4 = /* @__PURE__ */ eq(eqTerm);
  var show23 = /* @__PURE__ */ show(showInt);
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var li_2 = /* @__PURE__ */ li_(arrayToNodeData1);
  var div4 = /* @__PURE__ */ div3(arrayToNodeData2)(arrayToNodeData1);
  var ul2 = /* @__PURE__ */ ul(arrayToNodeData2)(arrayToNodeData1);
  var show32 = /* @__PURE__ */ show(showString);
  var ul_2 = /* @__PURE__ */ ul_(arrayToNodeData1);
  var showMode = function(mode) {
    return intercalate5(", ")(map24(show7)(mode));
  };
  var displayTerm = function(v) {
    return function(depth) {
      if (depth <= 0) {
        return [text("...")];
      }
      ;
      var parens4 = function(s) {
        return append14([span4([class$prime2("den-punct")])([text("(")])])(append14(s)([span4([class$prime2("den-punct")])([text(")")])]));
      };
      var go$prime = function(term1) {
        return displayTerm(term1)(depth - 1 | 0);
      };
      var displayRight = function(disp) {
        return function(v1) {
          if (v1 instanceof $$Set) {
            return disp(v1);
          }
          ;
          if (v1 instanceof Pair) {
            return disp(v1);
          }
          ;
          if (v1 instanceof Var) {
            return disp(v1);
          }
          ;
          if (v1 instanceof Con) {
            return disp(v1);
          }
          ;
          return parens4(disp(v1));
        };
      };
      var displayLeft = function(disp) {
        return function(v1) {
          if (v1 instanceof Lam) {
            return parens4(disp(v1));
          }
          ;
          return disp(v1);
        };
      };
      var go = function(v1) {
        if (v1 instanceof Con) {
          return [text(v1.value0)];
        }
        ;
        if (v1 instanceof Var) {
          return [text(showVar(v1.value0))];
        }
        ;
        if (v1 instanceof Lam) {
          return append14([span4([class$prime2("den-punct")])([text("\u03BB")])])(append14([text(showVar(v1.value0))])(append14([span4([class$prime2("den-punct")])([text(". ")])])(go$prime(v1.value1))));
        }
        ;
        if (v1 instanceof App2) {
          return append14(displayLeft(go$prime)(v1.value0))(append14([text(" ")])(displayRight(go$prime)(v1.value1)));
        }
        ;
        if (v1 instanceof Pair) {
          return append14([span4([class$prime2("den-punct")])([text("\u27E8")])])(append14(go$prime(v1.value0))(append14([span4([class$prime2("den-punct")])([text(", ")])])(append14(go$prime(v1.value1))([span4([class$prime2("den-punct")])([text("\u27E9")])]))));
        }
        ;
        if (v1 instanceof Fst) {
          return append14([span4([class$prime2("den-op")])([text("fst ")])])(displayRight(go)(v1.value0));
        }
        ;
        if (v1 instanceof Snd) {
          return append14([span4([class$prime2("den-op")])([text("snd ")])])(displayRight(go)(v1.value0));
        }
        ;
        if (v1 instanceof $$Set) {
          var v2 = unrollDom($$eval)(v1)(var_stock);
          var showNext = function(q) {
            return function(c2) {
              var $120 = $$null6(q);
              if ($120) {
                return q;
              }
              ;
              return append14(c2)(q);
            };
          };
          var getvar = function(v3) {
            if (v3 instanceof Cons) {
              return new Tuple(new Var(v3.value0), v3.value1);
            }
            ;
            if (v3 instanceof Nil) {
              return unsafeThrow("getvar error in displayTerm");
            }
            ;
            throw new Error("Failed pattern match at TDPretty (line 135, column 13 - line 135, column 41): " + [v3.constructor.name]);
          };
          var showDom = function(t) {
            return function(vs) {
              var v3 = getvar(vs);
              if (t instanceof Pair) {
                var $126 = eq4(t.value0)(new Con("_"));
                if ($126) {
                  return showDom(t.value1)(v3.value1);
                }
                ;
                return append14(go(v3.value0))(append14([span4([class$prime2("den-punct")])([text(" <- ")])])(append14(go$prime(t.value0))(showNext(showDom(t.value1)(v3.value1))([span4([class$prime2("den-punct")])([text(", ")])]))));
              }
              ;
              var $129 = eq4(t)(new Con("_"));
              if ($129) {
                return [];
              }
              ;
              return append14(go(v3.value0))(append14([span4([class$prime2("den-punct")])([text(" <- ")])])(go$prime(t)));
            };
          };
          var d = showDom(v2.value0)(v2.value1);
          return append14([span4([class$prime2("den-punct")])([text("[")])])(append14(go$prime($$eval(new App2(v1.value1, tuple(map24(Var.create)(v2.value1))))))(append14(function() {
            var $132 = $$null6(d);
            if ($132) {
              return d;
            }
            ;
            return append14([span4([class$prime2("den-punct")])([text(" | ")])])(d);
          }())([span4([class$prime2("den-punct")])([text("]")])])));
        }
        ;
        if (v1 instanceof Dom) {
          return append14([span4([class$prime2("den-op")])([text("dom ")])])(displayRight(go)(v1.value0));
        }
        ;
        if (v1 instanceof Rng) {
          return append14([span4([class$prime2("den-op")])([text("rng ")])])(displayRight(go)(v1.value0));
        }
        ;
        if (v1 instanceof Cct) {
          return append14([span4([class$prime2("den-op")])([text("concat ")])])(displayRight(go)(v1.value0));
        }
        ;
        if (v1 instanceof Spl) {
          return append14([span4([class$prime2("den-op")])([text("splitAt " + (show23(v1.value0) + " "))])])(displayRight(go)(v1.value1));
        }
        ;
        if (v1 instanceof Push) {
          return append14([span4([class$prime2("den-punct")])([text("(")])])(append14(go$prime(v1.value0))(append14([span4([class$prime2("den-punct")])([text(":")])])(append14(go$prime(v1.value1))([span4([class$prime2("den-punct")])([text(")")])]))));
        }
        ;
        if (v1 instanceof Proj) {
          return append14(go(v1.value1))([span4([class$prime2("den-punct")])([text("_" + show23(v1.value0))])]);
        }
        ;
        throw new Error("Failed pattern match at TDPretty (line 107, column 10 - line 179, column 78): " + [v1.constructor.name]);
      };
      return go(v);
    };
  };
  var displayVal = function(v) {
    return span4([class$prime2("den")])(displayTerm(evalFinal(semTerm(v)))(100));
  };
  var arrow = /* @__PURE__ */ text3(" -> ");
  var displayTy = function(b2) {
    return function(ty) {
      var parens4 = function(s) {
        return append14([span4([class$prime2("ty-punct")])([text("(")])])(append14(s)([span4([class$prime2("ty-punct")])([text(")")])]));
      };
      var displayLeft = function(v) {
        if (v instanceof Arr) {
          return parens4;
        }
        ;
        return identity10;
      };
      var ar = [span4([class$prime2("ty-punct")])([text(render3(100)(arrow))])];
      var go = function(v) {
        if (v instanceof E) {
          return [span4([class$prime2("atom")])([text("e")])];
        }
        ;
        if (v instanceof T) {
          return [span4([class$prime2("atom")])([text("t")])];
        }
        ;
        if (v instanceof G) {
          return [span4([class$prime2("atom")])([text("g")])];
        }
        ;
        if (v instanceof Eff) {
          return append14([displayF(b2)(v.value0)])(append14([text(function() {
            if (b2) {
              return "";
            }
            ;
            return "";
          }())])(displayParam(v.value1)));
        }
        ;
        if (v instanceof Arr) {
          return append14(displayLeft(v.value0)(go(v.value0)))(append14(ar)(go(v.value1)));
        }
        ;
        throw new Error("Failed pattern match at TDPretty (line 64, column 10 - line 69, column 59): " + [v.constructor.name]);
      };
      var displayParam = function(v) {
        if (v instanceof Arr) {
          return parens4(go(v));
        }
        ;
        if (v instanceof Eff) {
          return parens4(go(v));
        }
        ;
        return go(v);
      };
      var displayF = function(b1) {
        return function(f) {
          return span4([class$prime2("constructor")])(function() {
            var $160 = !b1;
            if ($160) {
              return [text(showNoIndices(f))];
            }
            ;
            if (f instanceof S) {
              return [text("S")];
            }
            ;
            if (f instanceof R) {
              return append14([text("R")])(displayParam(f.value0));
            }
            ;
            if (f instanceof W) {
              return append14([text("W")])(displayParam(f.value0));
            }
            ;
            if (f instanceof C) {
              return append14([text("C")])(append14(displayParam(f.value0))(displayParam(f.value1)));
            }
            ;
            if (f instanceof D) {
              return append14([text("D")])(append14(displayParam(f.value0))(displayParam(f.value1)));
            }
            ;
            if (f instanceof U) {
              return [text("U")];
            }
            ;
            throw new Error("Failed pattern match at TDPretty (line 74, column 11 - line 80, column 31): " + [f.constructor.name]);
          }());
        };
      };
      return span4([class$prime2("type")])(go(ty));
    };
  };
  var displayProof = function(dens) {
    return function(params) {
      return function(i) {
        return function(proof) {
          var html = function(v) {
            if (v.value1 instanceof Lex) {
              return li_2([div4([class$prime2("tf-nc")])(append14([displayTy(params)(v.value2)])(append14(function() {
                if (dens) {
                  return [br, displayVal(v.value1)];
                }
                ;
                return [];
              }())(append14([br])([span4([class$prime2("mode")])([text("Lex")])])))), ul2([class$prime2("parse-lex")])([li_2([span4([class$prime2("leaf")])([text(show32(v.value0))])])])]);
            }
            ;
            if (v.value1 instanceof Comb && (v.value3 instanceof Cons && (v.value3.value1 instanceof Cons && v.value3.value1.value1 instanceof Nil))) {
              return li_2([div4([class$prime2("tf-nc")])(append14([displayTy(params)(v.value2)])(append14(function() {
                if (dens) {
                  return [br, displayVal(v.value1)];
                }
                ;
                return [];
              }())(append14([br])([span4([class$prime2("mode")])([text(showMode(v.value1.value0))])])))), ul_2([html(v.value3.value0), html(v.value3.value1.value0)])]);
            }
            ;
            return li_2([span4([class$prime2("tf-nc")])([text("wrong number of daughters")])]);
          };
          return div4([class$prime2("tf-tree tf-gap-sm parse")])([span4([class$prime2("parse-number")])([text(show23(i + 1 | 0) + ".")]), ul_2([html(proof)])]);
        };
      };
    };
  };

  // output/Main/index.js
  var map25 = /* @__PURE__ */ map(functorMaybe);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable(foldableList);
  var arrayToNodeData3 = /* @__PURE__ */ arrayToNodeData(nodeDataToNodedata);
  var arrayToNodeData12 = /* @__PURE__ */ arrayToNodeData(htmlToHtml);
  var div5 = /* @__PURE__ */ div3(arrayToNodeData3)(arrayToNodeData12);
  var class$prime3 = /* @__PURE__ */ class$prime(stringClassList);
  var span_2 = /* @__PURE__ */ span_(arrayToNodeData12);
  var div_2 = /* @__PURE__ */ div_(arrayToNodeData12);
  var map111 = /* @__PURE__ */ map(functorArray);
  var span5 = /* @__PURE__ */ span(arrayToNodeData3)(arrayToNodeData12);
  var style2 = /* @__PURE__ */ style(/* @__PURE__ */ recordStyleList());
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorArray);
  var ul3 = /* @__PURE__ */ ul(arrayToNodeData3)(arrayToNodeData12);
  var li2 = /* @__PURE__ */ li(arrayToNodeData3)(arrayToNodeData12);
  var bind11 = /* @__PURE__ */ bind(bindArray);
  var fromFoldable1 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var or2 = /* @__PURE__ */ or(foldableArray)(heytingAlgebraBoolean);
  var flap2 = /* @__PURE__ */ flap(functorArray);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var lexParse2 = /* @__PURE__ */ lexParse(applicativeList);
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var input2 = /* @__PURE__ */ input(arrayToNodeData3);
  var p4 = /* @__PURE__ */ p(arrayToNodeData3)(arrayToNodeData12);
  var span1 = /* @__PURE__ */ span(stringToNodeData)(arrayToNodeData12);
  var button2 = /* @__PURE__ */ button(arrayToNodeData3)(arrayToNodeData12);
  var p1 = /* @__PURE__ */ p(stringToNodeData)(arrayToNodeData12);
  var show8 = /* @__PURE__ */ show(showInt);
  var min3 = /* @__PURE__ */ min(ordInt);
  var div1 = /* @__PURE__ */ div3(stringToNodeData)(arrayToNodeData12);
  var mapFlipped1 = /* @__PURE__ */ mapFlipped(functorMaybe);
  var append15 = /* @__PURE__ */ append(semigroupArray);
  var strong_2 = /* @__PURE__ */ strong_(stringToHtml);
  var PureLex = /* @__PURE__ */ function() {
    function PureLex2() {
    }
    ;
    PureLex2.value = new PureLex2();
    return PureLex2;
  }();
  var ProLex = /* @__PURE__ */ function() {
    function ProLex2() {
    }
    ;
    ProLex2.value = new ProLex2();
    return ProLex2;
  }();
  var DynLex = /* @__PURE__ */ function() {
    function DynLex2() {
    }
    ;
    DynLex2.value = new DynLex2();
    return DynLex2;
  }();
  var IndefLex = /* @__PURE__ */ function() {
    function IndefLex2() {
    }
    ;
    IndefLex2.value = new IndefLex2();
    return IndefLex2;
  }();
  var QuantLex = /* @__PURE__ */ function() {
    function QuantLex2() {
    }
    ;
    QuantLex2.value = new QuantLex2();
    return QuantLex2;
  }();
  var PushLex = /* @__PURE__ */ function() {
    function PushLex2() {
    }
    ;
    PushLex2.value = new PushLex2();
    return PushLex2;
  }();
  var DemoLex = /* @__PURE__ */ function() {
    function DemoLex2() {
    }
    ;
    DemoLex2.value = new DemoLex2();
    return DemoLex2;
  }();
  var MLComb = /* @__PURE__ */ function() {
    function MLComb2() {
    }
    ;
    MLComb2.value = new MLComb2();
    return MLComb2;
  }();
  var MRComb = /* @__PURE__ */ function() {
    function MRComb2() {
    }
    ;
    MRComb2.value = new MRComb2();
    return MRComb2;
  }();
  var ULComb = /* @__PURE__ */ function() {
    function ULComb2() {
    }
    ;
    ULComb2.value = new ULComb2();
    return ULComb2;
  }();
  var URComb = /* @__PURE__ */ function() {
    function URComb2() {
    }
    ;
    URComb2.value = new URComb2();
    return URComb2;
  }();
  var ZComb = /* @__PURE__ */ function() {
    function ZComb2() {
    }
    ;
    ZComb2.value = new ZComb2();
    return ZComb2;
  }();
  var AComb = /* @__PURE__ */ function() {
    function AComb2() {
    }
    ;
    AComb2.value = new AComb2();
    return AComb2;
  }();
  var EpsComb = /* @__PURE__ */ function() {
    function EpsComb2() {
    }
    ;
    EpsComb2.value = new EpsComb2();
    return EpsComb2;
  }();
  var JComb = /* @__PURE__ */ function() {
    function JComb2() {
    }
    ;
    JComb2.value = new JComb2();
    return JComb2;
  }();
  var DComb = /* @__PURE__ */ function() {
    function DComb2() {
    }
    ;
    DComb2.value = new DComb2();
    return DComb2;
  }();
  var PhraseInput = /* @__PURE__ */ function() {
    function PhraseInput2(value0) {
      this.value0 = value0;
    }
    ;
    PhraseInput2.create = function(value0) {
      return new PhraseInput2(value0);
    };
    return PhraseInput2;
  }();
  var TypeInput = /* @__PURE__ */ function() {
    function TypeInput2(value0) {
      this.value0 = value0;
    }
    ;
    TypeInput2.create = function(value0) {
      return new TypeInput2(value0);
    };
    return TypeInput2;
  }();
  var ToggleLex = /* @__PURE__ */ function() {
    function ToggleLex2() {
    }
    ;
    ToggleLex2.value = new ToggleLex2();
    return ToggleLex2;
  }();
  var ToggleDen = /* @__PURE__ */ function() {
    function ToggleDen2() {
    }
    ;
    ToggleDen2.value = new ToggleDen2();
    return ToggleDen2;
  }();
  var ToggleOpts = /* @__PURE__ */ function() {
    function ToggleOpts2() {
    }
    ;
    ToggleOpts2.value = new ToggleOpts2();
    return ToggleOpts2;
  }();
  var ToggleParams = /* @__PURE__ */ function() {
    function ToggleParams2() {
    }
    ;
    ToggleParams2.value = new ToggleParams2();
    return ToggleParams2;
  }();
  var AddLex = /* @__PURE__ */ function() {
    function AddLex2(value0) {
      this.value0 = value0;
    }
    ;
    AddLex2.create = function(value0) {
      return new AddLex2(value0);
    };
    return AddLex2;
  }();
  var LexChoice = /* @__PURE__ */ function() {
    function LexChoice2(value0) {
      this.value0 = value0;
    }
    ;
    LexChoice2.create = function(value0) {
      return new LexChoice2(value0);
    };
    return LexChoice2;
  }();
  var CombChoice = /* @__PURE__ */ function() {
    function CombChoice2(value0) {
      this.value0 = value0;
    }
    ;
    CombChoice2.create = function(value0) {
      return new CombChoice2(value0);
    };
    return CombChoice2;
  }();
  var eqLexName = {
    eq: function(x2) {
      return function(y) {
        if (x2 instanceof PureLex && y instanceof PureLex) {
          return true;
        }
        ;
        if (x2 instanceof ProLex && y instanceof ProLex) {
          return true;
        }
        ;
        if (x2 instanceof DynLex && y instanceof DynLex) {
          return true;
        }
        ;
        if (x2 instanceof IndefLex && y instanceof IndefLex) {
          return true;
        }
        ;
        if (x2 instanceof QuantLex && y instanceof QuantLex) {
          return true;
        }
        ;
        if (x2 instanceof PushLex && y instanceof PushLex) {
          return true;
        }
        ;
        if (x2 instanceof DemoLex && y instanceof DemoLex) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var elem4 = /* @__PURE__ */ elem2(eqLexName);
  var eq6 = /* @__PURE__ */ eq(eqLexName);
  var eqCombName = {
    eq: function(x2) {
      return function(y) {
        if (x2 instanceof MLComb && y instanceof MLComb) {
          return true;
        }
        ;
        if (x2 instanceof MRComb && y instanceof MRComb) {
          return true;
        }
        ;
        if (x2 instanceof ULComb && y instanceof ULComb) {
          return true;
        }
        ;
        if (x2 instanceof URComb && y instanceof URComb) {
          return true;
        }
        ;
        if (x2 instanceof ZComb && y instanceof ZComb) {
          return true;
        }
        ;
        if (x2 instanceof AComb && y instanceof AComb) {
          return true;
        }
        ;
        if (x2 instanceof EpsComb && y instanceof EpsComb) {
          return true;
        }
        ;
        if (x2 instanceof JComb && y instanceof JComb) {
          return true;
        }
        ;
        if (x2 instanceof DComb && y instanceof DComb) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var elem12 = /* @__PURE__ */ elem2(eqCombName);
  var eq12 = /* @__PURE__ */ eq(eqCombName);
  var unsInventory = /* @__PURE__ */ function() {
    return [new Tuple(JComb.value, addJ), new Tuple(DComb.value, addD)];
  }();
  var proofs = function(dictFoldable) {
    var prove2 = prove(dictFoldable);
    return function(l2) {
      return function(bins) {
        return function(uns) {
          return function(s) {
            return map25(fromFoldable8)(prove2(demoCFG)(l2)(bins)(uns)(s));
          };
        };
      };
    };
  };
  var proofs1 = /* @__PURE__ */ proofs(foldableList);
  var lexInventory = /* @__PURE__ */ function() {
    return [new Tuple(ProLex.value, fromFoldable8(proLex)), new Tuple(IndefLex.value, fromFoldable8(indefLex)), new Tuple(DynLex.value, fromFoldable8(dynLex)), new Tuple(QuantLex.value, fromFoldable8(quantLex)), new Tuple(PushLex.value, fromFoldable8(pushLex)), new Tuple(PureLex.value, fromFoldable8(pureLex)), new Tuple(DemoLex.value, fromFoldable8(demoLex))];
  }();
  var displayLexItem = function(b2) {
    return function(v) {
      var item = fromFoldable8(v.value1);
      return div5([class$prime3("lexitem")])(function() {
        var v1 = length(item);
        if (v1 === 1) {
          return [span_2([text(v.value0 + "  ")]), div_2(map111(function(v2) {
            return displayTy(b2)(v2.value1.value1);
          })(item))];
        }
        ;
        return [span5([style2({
          marginRight: "20px",
          minWidth: "61px"
        })])([text(v.value0 + "  ")]), div_2(mapFlipped3(item)(function(v2) {
          return ul3([style2({
            paddingLeft: "0px",
            marginBottom: "0px"
          })])([li2([style2({
            marginBottom: "0px"
          })])([displayTy(b2)(v2.value1.value1)])]);
        }))];
      }());
    };
  };
  var defLexes = /* @__PURE__ */ function() {
    return [PureLex.value, ProLex.value, IndefLex.value];
  }();
  var defCombs = /* @__PURE__ */ function() {
    return [MRComb.value, MLComb.value, AComb.value, JComb.value];
  }();
  var init3 = /* @__PURE__ */ function() {
    return {
      currentPhrase: "",
      typeOfInterest: $$const(true),
      currentProofs: new Just([]),
      customLex: [],
      lexFeedback: Nothing.value,
      opts: {
        showOpts: true,
        showDens: true,
        showParams: false,
        showLex: true,
        lexItems: function(l2) {
          var $137 = elem4(l2)(defLexes);
          if ($137) {
            return true;
          }
          ;
          return false;
        },
        combs: function(c2) {
          var $138 = elem12(c2)(defCombs);
          if ($138) {
            return true;
          }
          ;
          return false;
        }
      }
    };
  }();
  var buildUns = function(m2) {
    return bind11(unsInventory)(function(v) {
      var $140 = m2.opts.combs(v.value0);
      if ($140) {
        return [v.value1];
      }
      ;
      return [];
    });
  };
  var buildLex = function(m2) {
    return concat(cons2(m2.customLex)(map111(function(v) {
      var $144 = m2.opts.lexItems(v.value0);
      if ($144) {
        return v.value1;
      }
      ;
      return [];
    })(lexInventory)));
  };
  var binsInventory = function(dictFunctor) {
    var addML2 = addML(dictFunctor);
    return function(dictApplicative) {
      var Functor0 = dictApplicative.Apply0().Functor0();
      return [new Tuple(MLComb.value, addML2(dictApplicative)), new Tuple(MRComb.value, addMR(Functor0)(dictApplicative)), new Tuple(ULComb.value, addUL(Functor0)(dictApplicative)), new Tuple(URComb.value, addUR(Functor0)(dictApplicative)), new Tuple(ZComb.value, addZ(Functor0)(dictApplicative)), new Tuple(AComb.value, addA(Functor0)(dictApplicative)), new Tuple(EpsComb.value, addEps(Functor0)(dictApplicative))];
    };
  };
  var buildBins = function(dictFunctor) {
    var binsInventory1 = binsInventory(dictFunctor);
    return function(dictApplicative) {
      var binsInventory2 = binsInventory1(dictApplicative);
      return function(m2) {
        return bind11(binsInventory2)(function(v) {
          var $148 = m2.opts.combs(v.value0);
          if ($148) {
            return [v.value1];
          }
          ;
          return [];
        });
      };
    };
  };
  var buildBins1 = /* @__PURE__ */ buildBins(/* @__PURE__ */ functorStateT(functorIdentity))(/* @__PURE__ */ applicativeStateT(monadIdentity));
  var update = function(model) {
    return function(v) {
      if (v instanceof PhraseInput && v.value0.value0 === "Enter") {
        return {
          currentPhrase: '"' + (v.value0.value1 + '"'),
          typeOfInterest: model.typeOfInterest,
          currentProofs: proofs1(fromFoldable1(buildLex(model)))(fromFoldable1(buildBins1(model)))(fromFoldable1(buildUns(model)))(v.value0.value1),
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: model.opts
        };
      }
      ;
      if (v instanceof PhraseInput) {
        return model;
      }
      ;
      if (v instanceof TypeInput) {
        var v1 = tyParse(v.value0.value1);
        if (v1 instanceof Left) {
          return {
            currentPhrase: model.currentPhrase,
            typeOfInterest: $$const(true),
            currentProofs: model.currentProofs,
            customLex: model.customLex,
            lexFeedback: model.lexFeedback,
            opts: model.opts
          };
        }
        ;
        if (v1 instanceof Right) {
          return {
            currentPhrase: model.currentPhrase,
            typeOfInterest: function(p22) {
              return or2(flap2(map111(hasType)(v1.value0))(p22));
            },
            currentProofs: model.currentProofs,
            customLex: model.customLex,
            lexFeedback: model.lexFeedback,
            opts: model.opts
          };
        }
        ;
        throw new Error("Failed pattern match at Main (line 131, column 5 - line 133, column 77): " + [v1.constructor.name]);
      }
      ;
      if (v instanceof ToggleLex) {
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: model.opts.showOpts,
            showDens: model.opts.showDens,
            showParams: model.opts.showParams,
            showLex: !model.opts.showLex,
            lexItems: model.opts.lexItems,
            combs: model.opts.combs
          }
        };
      }
      ;
      if (v instanceof ToggleDen) {
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: model.opts.showOpts,
            showDens: !model.opts.showDens,
            showParams: model.opts.showParams,
            showLex: model.opts.showLex,
            lexItems: model.opts.lexItems,
            combs: model.opts.combs
          }
        };
      }
      ;
      if (v instanceof ToggleParams) {
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: model.opts.showOpts,
            showDens: model.opts.showDens,
            showParams: !model.opts.showParams,
            showLex: model.opts.showLex,
            lexItems: model.opts.lexItems,
            combs: model.opts.combs
          }
        };
      }
      ;
      if (v instanceof ToggleOpts) {
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: !model.opts.showOpts,
            showDens: model.opts.showDens,
            showParams: model.opts.showParams,
            showLex: model.opts.showLex,
            lexItems: model.opts.lexItems,
            combs: model.opts.combs
          }
        };
      }
      ;
      if (v instanceof AddLex && v.value0.value0 === "Enter") {
        var v1 = lexParse2(v.value0.value1);
        if (v1 instanceof Left) {
          return {
            currentPhrase: model.currentPhrase,
            typeOfInterest: model.typeOfInterest,
            currentProofs: model.currentProofs,
            customLex: model.customLex,
            lexFeedback: new Just(v1.value0),
            opts: model.opts
          };
        }
        ;
        if (v1 instanceof Right) {
          return {
            currentPhrase: model.currentPhrase,
            typeOfInterest: model.typeOfInterest,
            currentProofs: model.currentProofs,
            customLex: cons2(v1.value0)(model.customLex),
            lexFeedback: Nothing.value,
            opts: model.opts
          };
        }
        ;
        throw new Error("Failed pattern match at Main (line 144, column 5 - line 146, column 83): " + [v1.constructor.name]);
      }
      ;
      if (v instanceof AddLex) {
        return model;
      }
      ;
      if (v instanceof LexChoice) {
        var $$switch = function(n1) {
          return function(items) {
            return function(l2) {
              return function() {
                var $173 = eq6(l2)(n1);
                if ($173) {
                  return not2;
                }
                ;
                return identity11;
              }()(items(l2));
            };
          };
        };
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: model.opts.showOpts,
            showDens: model.opts.showDens,
            showParams: model.opts.showParams,
            showLex: model.opts.showLex,
            lexItems: $$switch(v.value0)(model.opts.lexItems),
            combs: model.opts.combs
          }
        };
      }
      ;
      if (v instanceof CombChoice) {
        var $$switch = function(n1) {
          return function(items) {
            return function(c2) {
              return function() {
                var $175 = eq12(c2)(n1);
                if ($175) {
                  return not2;
                }
                ;
                return identity11;
              }()(items(c2));
            };
          };
        };
        return {
          currentPhrase: model.currentPhrase,
          typeOfInterest: model.typeOfInterest,
          currentProofs: model.currentProofs,
          customLex: model.customLex,
          lexFeedback: model.lexFeedback,
          opts: {
            showOpts: model.opts.showOpts,
            showDens: model.opts.showDens,
            showParams: model.opts.showParams,
            showLex: model.opts.showLex,
            lexItems: model.opts.lexItems,
            combs: $$switch(v.value0)(model.opts.combs)
          }
        };
      }
      ;
      throw new Error("Failed pattern match at Main (line 121, column 16 - line 155, column 78): " + [v.constructor.name]);
    };
  };
  var addSwitch = function(dictToNode) {
    var span_1 = span_(dictToNode);
    return function(action) {
      return function(toggle) {
        return function(v) {
          return div_2([input2([class$prime3("opt-switch"), type$prime("checkbox"), checked(toggle(v.value1)), onClick(action(v.value1))]), span_1(v.value0)]);
        };
      };
    };
  };
  var addSwitch1 = /* @__PURE__ */ addSwitch(arrayToNodeData12);
  var addLexText = function(m2) {
    return p4([style2({
      marginBottom: "0px"
    })])([text("Add item: "), span1("lexFeedback")([text(m2)])]);
  };
  var addLexInput = /* @__PURE__ */ function() {
    return input2([type$prime("text"), id("lexname"), placeholder("(name, cat, type)"), onKeyup(AddLex.create)]);
  }();
  var view = function(model) {
    return div5([id("parser")])([input2([type$prime("text"), id("phraseInput"), placeholder("Enter a sentence"), onKeyup(PhraseInput.create)]), input2([type$prime("text"), id("typeInput"), placeholder("Filter by type"), onKeyup(TypeInput.create)]), button2([id("lex-button"), onClick(ToggleLex.value)])([text(function() {
      if (model.opts.showLex) {
        return "hide";
      }
      ;
      return "show";
    }() + " lexicon")]), button2([id("opts-button"), onClick(ToggleOpts.value)])([text("\u2261")]), p1("current")([text("Showing "), span5([style2({
      color: "var(--accent)"
    })])([text(show8(min3(200)(maybe(0)(function() {
      var $185 = filter(model.typeOfInterest);
      return function($186) {
        return length($185($186));
      };
    }())(model.currentProofs))))]), text(" of "), span5([style2({
      color: "var(--accent)"
    })])([text(show8(maybe(0)(length)(model.currentProofs)))]), text(" parses for: " + model.currentPhrase)]), div1("content")([div1("parses")(fromMaybe([text("No parse")])(mapFlipped1(model.currentProofs)(function() {
      var $187 = mapWithIndex(displayProof(model.opts.showDens)(model.opts.showParams));
      var $188 = take(100);
      var $189 = filter(model.typeOfInterest);
      return function($190) {
        return $187($188($189($190)));
      };
    }()))), div5([id("lexicon"), style2({
      display: function() {
        if (model.opts.showLex) {
          return "block";
        }
        ;
        return "none";
      }()
    })])(cons2(addLexText(fromMaybe("")(model.lexFeedback)))(cons2(addLexInput)(map111(displayLexItem(model.opts.showParams))(buildLex(model))))), div5([id("options"), style2({
      display: function() {
        if (model.opts.showOpts) {
          return "block";
        }
        ;
        return "none";
      }()
    })])([div5([id("denInput"), class$prime3("opt-group")])([div_2([input2([class$prime3("opt-switch"), type$prime("checkbox"), checked(true), onClick(ToggleDen.value)]), span_2([text("show meanings")])]), div_2([input2([class$prime3("opt-switch"), type$prime("checkbox"), checked(false), onClick(ToggleParams.value)]), span_2([text("show eff params")])])]), div5([id("lexInventory"), class$prime3("opt-group")])(append15([text("Select fragments:")])(map111(addSwitch1(LexChoice.create)(function(v) {
      return elem4(v)(defLexes);
    }))([new Tuple([text("pure")], PureLex.value), new Tuple([text("pro")], ProLex.value), new Tuple([text("indef")], IndefLex.value), new Tuple([text("dyn")], DynLex.value), new Tuple([text("quant")], QuantLex.value), new Tuple([text("push")], PushLex.value), new Tuple([text("demo")], DemoLex.value)]))), div5([id("combsInventory"), class$prime3("opt-group")])(append15([text("Select combinators:")])(map111(addSwitch1(CombChoice.create)(function(v) {
      return elem12(v)(defCombs);
    }))([new Tuple([strong_2("R"), text(" (map right)")], MRComb.value), new Tuple([strong_2("L"), text(" (map left)")], MLComb.value), new Tuple([strong_2("\xDA"), text(" (unit right)")], URComb.value), new Tuple([strong_2("\xD9"), text(" (unit left)")], ULComb.value), new Tuple([strong_2("A"), text(" (apply)")], AComb.value), new Tuple([strong_2("E"), text(" (counit)")], EpsComb.value), new Tuple([strong_2("J"), text(" (join)")], JComb.value), new Tuple([strong_2("D"), text(" (lower)")], DComb.value)])))])])]);
  };
  var main = /* @__PURE__ */ mount_2("#home")({
    init: init3,
    subscribe: [],
    update,
    view
  });

  // <stdin>
  main();
})();
