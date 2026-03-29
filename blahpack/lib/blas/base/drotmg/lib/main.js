
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var drotmg = require( './drotmg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( drotmg, 'ndarray', ndarray );


// EXPORTS //

module.exports = drotmg;
