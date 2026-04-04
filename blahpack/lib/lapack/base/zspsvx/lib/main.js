

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zspsvx = require( './zspsvx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zspsvx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zspsvx;
