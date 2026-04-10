'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zggevx = require( './zggevx.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zggevx, 'ndarray', ndarray );


// EXPORTS //

module.exports = zggevx;
