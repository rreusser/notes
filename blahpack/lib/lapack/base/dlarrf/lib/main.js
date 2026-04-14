
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarrf = require( './dlarrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarrf;
