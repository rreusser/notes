

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zsteqr = require( './zsteqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsteqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsteqr;
