'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpr2 = require( './zhpr2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpr2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpr2;
