
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarfgp = require( './dlarfgp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfgp, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfgp;
