'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlascl2 = require( './zlascl2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlascl2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlascl2;
