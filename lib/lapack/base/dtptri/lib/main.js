
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtptri = require( './dtptri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtptri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtptri;
