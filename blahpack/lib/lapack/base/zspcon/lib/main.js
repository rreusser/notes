'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zspcon = require( './zspcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zspcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zspcon;
