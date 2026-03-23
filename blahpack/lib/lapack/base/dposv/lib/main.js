

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dposv = require( './dposv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dposv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dposv;
