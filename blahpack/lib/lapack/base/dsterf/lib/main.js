

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsterf = require( './dsterf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsterf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsterf;
