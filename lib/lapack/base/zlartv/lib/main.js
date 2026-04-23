'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlartv = require( './zlartv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlartv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlartv;
