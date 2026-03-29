'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgtcon = require( './dgtcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgtcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgtcon;
