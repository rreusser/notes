'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgtcon = require( './zgtcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgtcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgtcon;
