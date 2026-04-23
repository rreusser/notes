
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlag2s = require( './dlag2s.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlag2s, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlag2s;
