function DNRM2(n, x, incx) {
    var notbig;
    var DNRM2;
    var sumsq;
    var abig;
    var amed;
    var asml;
    var maxN;
    var sbig;
    var ssml;
    var tbig;
    var tsml;
    var ymax;
    var ymin;
    var zero;
    var one;
    var scl;
    var ax;
    var ix;
    var wp;
    var i;
    DNRM2 = zero;
    if (n <= 0)
        return;
    scl = one;
    sumsq = zero;
    notbig = true;
    asml = zero;
    amed = zero;
    abig = zero;
    ix = 1;
    if (incx < 0)
        ix = 1 - (n - 1) * incx;
    for (i = 1; i <= n; i += 1) {
        ax = abs(x[ix]);
        if (ax > tbig) {
            abig = abig + (ax * sbig) ** 2;
            notbig = false;
        } else if (ax < tsml) {
            if (notbig)
                asml = asml + (ax * ssml) ** 2;
        } else {
            amed = amed + ax ** 2;
        }
        ix = ix + incx;
    }
    if (abig > zero) {
        if (amed > zero || amed > maxN || amed != amed) {
            abig = abig + amed * sbig * sbig;
        }
        scl = one / sbig;
        sumsq = abig;
    } else if (asml > zero) {
        if (amed > zero || amed > maxN || amed != amed) {
            amed = sqrt(amed);
            asml = sqrt(asml) / ssml;
            if (asml > amed) {
                ymin = amed;
                ymax = asml;
            } else {
                ymin = asml;
                ymax = amed;
            }
            scl = one;
            sumsq = ymax ** 2 * (one + (ymin / ymax) ** 2);
        } else {
            scl = one / ssml;
            sumsq = asml;
        }
    } else {
        scl = one;
        sumsq = amed;
    }
    DNRM2 = scl * sqrt(sumsq);
    return;
}
