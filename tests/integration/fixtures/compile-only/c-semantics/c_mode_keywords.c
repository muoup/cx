struct KeywordFields
{
    int where;
    int move;
    int match;
    int safe;
    int public;
    int private;
};

int sum_keyword_fields(struct KeywordFields *fields)
{
    return fields->where
         + fields->move
         + fields->match
         + fields->safe
         + fields->public
         + fields->private;
}
