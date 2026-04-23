'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgerq2 = require( './zgerq2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgerq2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgerq2;
