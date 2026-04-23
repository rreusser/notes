
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsptri = require( './zsptri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsptri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsptri;
