

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zungqr = require( './zungqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungqr;
