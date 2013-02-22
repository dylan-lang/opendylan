.. raw:: html

   <div class="row">
     <div class="span8">

     <div id="code-carousel" class="carousel slide">
       <ol class="carousel-indicators">
         <li data-target="#code-carousel" data-slide-to="0" class="active"></li>
         <li data-target="#code-carousel" data-slide-to="1"></li>
         <li data-target="#code-carousel" data-slide-to="2"></li>
       </ol>
       <div class="carousel-inner">
         <div class="item active">
.. code-block:: dylan

  define class <vehicle> (<object>)
    slot owner :: <string>,
      init-keyword: owner:,
      init-value: "Northern Motors";
  end class <vehicle>;

  define class <car> (<vehicle>)
  end class <car>;

  define class <truck> (<vehicle>)
    slot capacity,
      required-init-keyword: tons:;
  end class <truck>;

.. raw:: html

         </div>
         <div class="item">
.. code-block:: dylan

  define generic tax
      (v :: <vehicle>) => (tax-in-dollars :: <float>);

  define method tax
      (v :: <vehicle>) => (tax-in-dollars :: <float>)
    100.00
  end;

  define method tax
      (c :: <car>) => (tax-in-dollars :: <float>)
    50.0;
  end method;

  define method tax
      (t :: <truck>) => (tax-in-dollars :: <float>)
    // standard vehicle tax plus $10/ton
    next-method() + t.capacity * 10.0;
  end method;

.. raw:: html

         </div>
         <div class="item">
.. code-block:: dylan

  define function make-fibonacci()
    let n = 0;
    let m = 1;
    method ()
      let result = n + m;
      n := m;
      m := result  // return value
    end
  end;

  define constant fib = make-fibonacci();

  for (i from 1 to 15)
    format-out("%d ", fib())
  end;

.. raw:: html

         </div>
       </div>
       <a class="carousel-control left" href="#code-carousel" data-slide="prev">&lsaquo;</a>
       <a class="carousel-control right" href="#code-carousel" data-slide="next">&rsaquo;</a>
     </div>
     <h2>Recent News</h2>

     <div class="alert alert-block alert-info">
       <p>Keep up to date by subscribing to our <a href="rss.xml">RSS
       Feed <img src="_static/feed-icon-14x14.png" alt=""></a> or
       joining our <a href="community/index.html#mailing-lists">mailing
       lists</a>.</p>
     </div>

.. include:: news/recent.rst.inc

.. raw:: html

       <div class="pull-right"><a href="news/index.html" class="btn btn-primary btn-large">All news &raquo;</a></div>
     </div>
     <div class="span4">

     <h2>Get Started</h2>

The `Dylan Foundry <http://dylanfoundry.org/>`_ has written about
`getting started with the 2012.1 release <http://dylanfoundry.org/2012/12/20/getting-started-with-opendylan-20121/>`_ or just go ahead and `download </download/index.html>`_ it.

.. raw:: html

     <h2>Learn about Dylan</h2>

Dylan has a large amount of documentation available:

* `Introduction to Dylan <http://opendylan.org/documentation/intro-dylan/>`_:
   A tutorial written for those with solid programming
   experience in C++ or another object-oriented, static language. It
   provides a gentler introduction to Dylan than does the Dylan
   Reference Manual (DRM).
* `Dylan Programming Guide <http://opendylan.org/books/dpg/>`_:
   A book length Dylan tutorial.
* `Dylan Reference Manual <http://opendylan.org/books/drm/>`_:
   The official definition of the Dylan language and standard library.
* `Open Dylan Documentation <http://opendylan.org/documentation/>`_:
   All of the Open Dylan documentation.

.. raw:: html

     </div>
   </div>

.. toctree::
   :maxdepth: 1
   :hidden:
   :glob:

   */*
   articles/*/*
   documentation/cheatsheets/*
   news/*/*/*/*
   community/gsoc/*

