'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlahef = require( './zlahef.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlahef, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlahef;
