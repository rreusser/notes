'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlar2v = require( './zlar2v.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlar2v, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlar2v;
