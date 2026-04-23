'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlahqr = require( './zlahqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlahqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlahqr;
