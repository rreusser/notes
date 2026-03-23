

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlarfg = require( './dlarfg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarfg, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarfg;
