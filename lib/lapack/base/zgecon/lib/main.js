'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgecon = require( './zgecon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgecon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgecon;
